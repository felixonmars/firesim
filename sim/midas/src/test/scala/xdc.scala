//See LICENSE for license details.

package goldengate.tests

import midas.fpgautils._

import scala.reflect.runtime.universe.{MethodSymbol, TypeTag, typeOf}

class XDCSpec extends org.scalatest.flatspec.AnyFlatSpec {
  object SerializeAndCheck {
    def apply[T <: TCLExpression : TypeTag](expr: T, reference: String): Unit = {
      val str = TCLSerializer(expr)
      println(str)
      assert(str == reference)
    }
  }
  "GetPins" should "serialize" in {
    //SerializeAndCheck(GetPins(TCLIntLiteral(5)), "get_pins 5")
    SerializeAndCheck(GetPins(GetCells(TCLIntLiteral(5))), "get_pins [get_cells 5]")
  }
}
