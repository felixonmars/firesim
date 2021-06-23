//See LICENSE for license details.

package midas.fpgautils

import midas.targetutils.ReferenceTargetRenamer
import firrtl.annotations._

import scala.reflect.runtime.universe.{MethodSymbol, TypeTag, typeOf}

sealed trait TCLExpression extends Product

sealed trait TCLLiteral[T] extends TCLExpression { def value: T }
case class TCLIntLiteral(value: Int) extends TCLLiteral[Int]
case class TCLStringLiteral(value: String) extends TCLLiteral[String]

//case class AbsoluteRef(target: ReferenceTarget) extends TCLExpression
//case class HierarchalRef(target: ReferenceTarget) extends TCLExpression

sealed trait TCLCommand extends TCLExpression {
  def name: String
}
sealed trait NetlistQuery extends TCLCommand {
  def expr: TCLExpression
  def hierarchical: Boolean
  def filter: Option[String]
  def objectName: String
  def name = s"get_${objectName}"
  def toTCL = TCLSerializer(this)
}


case class GetPins(expr: TCLExpression, hierarchical: Boolean = false, filter: Option[String] = None) extends NetlistQuery { def objectName = "pins" }
case class GetClocks(expr: TCLExpression, hierarchical: Boolean = false, filter: Option[String] = None) extends NetlistQuery { def objectName = "clocks" }
case class GetCells(expr: TCLExpression, hierarchical: Boolean = false, filter: Option[String] = None) extends NetlistQuery { def objectName = "cells" }


trait XDCAnnotation extends TCLExpression { this: Annotation =>
  //def updatedParametersExact(renames: RenameMap): {
  //  val renamer = ReferenceTargetRenamer(renames)
  //  productIterator.map {
  //    case p: Pin => p.copy(renamer.exactRename(p.target))
  //    case o => o
  //  }
  //}
}

object TCLSerializer {
  def apply[T <: TCLExpression : TypeTag](anno: T): String = {
    val argNames = typeOf[T].members.collect {
      case m: MethodSymbol if m.isCaseAccessor => m
    }.toList

    val argStrs = (for ((name, value) <- argNames.zip(anno.productIterator.toIterable)) yield {
      println((name,value))
      value match {
        // Raw Booleans are treated as unary kwarg arguments
        case b: Boolean => if (b) Some(s"-${name}") else None
        // Iterable collections are treated as kwargs,
        case s: Option[TCLExpression] => s.map { v => (s"-${name} [ ${TCLSerializer(v)} ]") }
        case s: Iterable[TCLExpression] => if (s.nonEmpty) {
          val listItems = s.map(x => s"[ ${TCLSerializer(x)} ]").mkString(" ")
          Some(s"-${name} { $listItems }")
        } else {
          None
        }

        // Unwrapped types are positional arguments
        case cmd: TCLCommand => 
          println(s"Calling $cmd")
          Some(s"[ ${TCLSerializer(cmd)} ]")
        case lit: TCLLiteral[Any] => Some(TCLSerializer(lit))
        case o => throw new Exception(s"Unexpected type: ${o}")
      }
    }).flatten

    val argString = argStrs.mkString(" ")
    anno match {
      case lit: TCLLiteral[_] => lit.value.toString
      case cmd: TCLCommand => s"${cmd.name} ${argString}"
    }
  }
}

case class MultiCyclePath(
  multiplier: Int,
  from: Seq[NetlistQuery],
  to: Seq[NetlistQuery],
  rise: Boolean = false,
  fall: Boolean = false,
) extends TCLExpression {
  def name = "set_multicycle_path"
}
