package strober

import Chisel._
import junctions._

import scala.collection.mutable.ArrayBuffer
import scala.collection.immutable.ListSet

class SimDecoupledIO[+T <: Data](gen: T) extends Bundle {
  val ready  = Bool(INPUT)
  val valid  = Bool(OUTPUT)
  val target = Decoupled(gen)
  def fire(dummy: Int = 0): Bool = ready && valid
  override def cloneType: this.type = new SimDecoupledIO(gen).asInstanceOf[this.type] 
}

object SimMemIO {
  private val mems = ArrayBuffer[MemIO]()
  def apply(mem: MemIO) { mems += mem }
  def apply(i: Int) = mems(i)
  def size = mems.size
  def contains(wire: Bits) = mems exists (_.flatten.unzip._2 contains wire) 
  def zipWithIndex = mems.toList.zipWithIndex
}

class SimMemIO extends MIFBundle {
  val req_cmd  = new SimDecoupledIO(new MemReqCmd)
  val req_data = new SimDecoupledIO(new MemData)
  val resp     = (new SimDecoupledIO(new MemResp)).flip

  def <>(mIo: MemIO, wIo: SimWrapperIO) {
    val ins = wIo.inMap filter (SimMemIO contains _._1) flatMap wIo.getIns
    // Target Connection
    def targetConnect[T <: Bits](target: T, wire: T) = wire match {
      case _: Bool if wire.dir == OUTPUT => target := wIo.getOuts(wire).head.bits
      case _: Bool if wire.dir == INPUT => wIo.getIns(wire).head.bits := target
      case _ if wire.dir == OUTPUT => target := Vec(wIo.getOuts(wire) map (_.bits)).toBits
      case _ if wire.dir == INPUT => wIo.getIns(wire).zipWithIndex foreach {
        case (in, i) => in.bits := target.toUInt >> UInt(i*wIo.channelWidth) }
    }
    targetConnect(req_cmd.target.bits.addr, mIo.req_cmd.bits.addr)
    targetConnect(req_cmd.target.bits.tag,  mIo.req_cmd.bits.tag)
    targetConnect(req_cmd.target.bits.rw,   mIo.req_cmd.bits.rw)
    targetConnect(req_cmd.target.valid,     mIo.req_cmd.valid)
    targetConnect(req_cmd.target.ready,     mIo.req_cmd.ready)
    
    targetConnect(req_data.target.bits.data, mIo.req_data.bits.data)
    targetConnect(req_data.target.valid,     mIo.req_data.valid)
    targetConnect(req_data.target.ready,     mIo.req_data.ready)

    targetConnect(resp.target.bits.data, mIo.resp.bits.data)
    targetConnect(resp.target.bits.tag,  mIo.resp.bits.tag)
    targetConnect(resp.target.valid,     mIo.resp.valid)
    targetConnect(resp.target.ready,     mIo.resp.ready)

    // Host Connection
    def hostConnect(res: Bool, arg: (String, Bits), ready: Bool) = arg match { 
      case (_, wire) if wire.dir == INPUT =>
        val ins = wIo.getIns(wire)
        ins foreach (_.valid := ready)
        (ins foldLeft res)(_ && _.ready)
      case (_, wire) if wire.dir == OUTPUT =>
        val outs = wIo.getOuts(wire)
        outs foreach (_.ready := ready)
        (outs foldLeft res)(_ && _.valid)
    }
    req_cmd.valid  := (mIo.req_cmd.flatten foldLeft Bool(true))(hostConnect(_, _, req_cmd.ready))
    req_data.valid := (mIo.req_data.flatten foldLeft Bool(true))(hostConnect(_, _, req_data.ready))
    resp.ready     := (mIo.resp.flatten foldLeft Bool(true))(hostConnect(_, _, resp.valid))
  }
}

class ChannelMemIOConverter extends MIFModule {
  val io = new Bundle {
    val sim_mem  = (new SimMemIO).flip
    val host_mem =  new MemIO
  }

  val req_cmd_buf  = Module(new Queue(new MemReqCmd, 2, flow=true))
  val req_data_buf = Module(new Queue(new MemData,   2, flow=true))
  val resp_buf     = Module(new Queue(new MemResp,   2, flow=true))

  io.sim_mem.req_cmd.target.ready := Bool(true)
  io.sim_mem.req_cmd.ready     := io.sim_mem.req_cmd.valid && req_cmd_buf.io.enq.ready &&
                                  io.sim_mem.req_data.valid && req_data_buf.io.enq.ready
  req_cmd_buf.io.enq.bits.addr := io.sim_mem.req_cmd.target.bits.addr
  req_cmd_buf.io.enq.bits.tag  := io.sim_mem.req_cmd.target.bits.tag
  req_cmd_buf.io.enq.bits.rw   := io.sim_mem.req_cmd.target.bits.rw
  req_cmd_buf.io.enq.valid     := io.sim_mem.req_cmd.target.valid && io.sim_mem.req_cmd.ready

  io.sim_mem.req_data.target.ready := Bool(true)
  io.sim_mem.req_data.ready     := io.sim_mem.req_cmd.ready
  req_data_buf.io.enq.bits.data := io.sim_mem.req_data.target.bits.data
  req_data_buf.io.enq.valid     := io.sim_mem.req_data.target.valid && io.sim_mem.req_data.ready

  resp_buf.io.deq.ready := io.sim_mem.resp.ready && io.sim_mem.resp.target.ready 
  io.sim_mem.resp.valid := io.sim_mem.resp.ready && (resp_buf.io.deq.valid || 
    io.sim_mem.req_cmd.ready && (!io.sim_mem.req_cmd.target.valid || io.sim_mem.req_cmd.target.bits.rw))
  io.sim_mem.resp.target.bits.data := resp_buf.io.deq.bits.data
  io.sim_mem.resp.target.bits.tag  := resp_buf.io.deq.bits.tag
  io.sim_mem.resp.target.valid     := resp_buf.io.deq.valid

  io.host_mem.req_cmd  <> req_cmd_buf.io.deq
  io.host_mem.req_data <> req_data_buf.io.deq
  resp_buf.io.enq      <> io.host_mem.resp
}

class MemArbiter(n: Int) extends Module {
  val io = new Bundle {
    val ins = Vec.fill(n){(new MemIO).flip}
    val out = new MemIO  
  }
  val s_READY :: s_READ :: s_WRITE :: Nil = Enum(UInt(), 3)
  val state = RegInit(s_READY)
  val chosen = RegInit(UInt(n-1))

  io.out.req_cmd.bits := io.ins(chosen).req_cmd.bits
  io.out.req_cmd.valid := io.ins(chosen).req_cmd.valid && state === s_READY
  io.ins foreach (_.req_cmd.ready := Bool(false))
  io.ins.zipWithIndex foreach { case (in, i) => 
    in.req_cmd.ready := io.out.req_cmd.ready && chosen === UInt(i)
  }

  io.out.req_data.bits := io.ins(chosen).req_data.bits
  io.out.req_data.valid := io.ins(chosen).req_data.valid && state =/= s_READ 
  io.ins foreach (_.req_data.ready := Bool(false))
  io.ins.zipWithIndex foreach { case (in, i) => 
    in.req_data.ready := io.out.req_data.ready && chosen === UInt(i) 
  }

  io.ins.zipWithIndex foreach { case (in, i) => 
    in.resp.bits := io.out.resp.bits 
    in.resp.valid := io.out.resp.valid && chosen === UInt(i) 
  }
  io.out.resp.ready := io.ins(chosen).resp.ready 

  switch(state) {
    is(s_READY) {
      when(!io.ins(chosen).req_cmd.valid && !io.ins(chosen).req_data.valid) {
        chosen := Mux(chosen.orR, chosen - UInt(1), UInt(n-1))
      }.elsewhen(io.ins(chosen).req_cmd.fire() && !io.ins(chosen).req_cmd.bits.rw) {
        state := s_READ
      }.elsewhen(io.ins(chosen).req_cmd.fire() && !io.ins(chosen).req_data.fire()) {
        state := s_WRITE
      }
    }
    is(s_READ) {
      state := Mux(io.ins(chosen).resp.fire(), s_READY, s_READ)
    }
    is(s_WRITE) {
      state := Mux(io.ins(chosen).req_data.fire(), s_READY, s_WRITE)
    }
  }
}
