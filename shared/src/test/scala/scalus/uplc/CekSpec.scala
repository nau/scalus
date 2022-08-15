package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultUni.{ByteString, asConstant}
import scalus.uplc.Term.*

import java.io.ByteArrayInputStream
import scala.io.Source.fromFile
class CekSpec extends AnyFunSuite:
  test("Scalus") {
    val h = Const(asConstant("Hello"))
    val id = LamAbs("x", Var("x"))
    val app = Apply(id, h)
    assert(Cek.evalUPLC(app) == h)
  }

  def run(code: String) = {
    for
      program <- UplcParser.parseProgram(code)
      evaled = Cek.evalUPLCProgram(program)
    do println(evaled.pretty.render(80))
  }

  def eval(code: String): Term = {
    UplcParser.parseProgram(code).map(Cek.evalUPLCProgram).getOrElse(sys.error("Parse error"))
  }

  test("EqualsInteger") {
    def check(code: String, result: Boolean) =
      assert(eval(code) == Const(asConstant(result)))

    check("(program 1.0.0 [[(builtin equalsInteger) (con integer 0)] (con integer 0)])", true)
    check("(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con integer 1)])", true)
    check(
      "(program 1.0.0 [[(builtin equalsInteger) (con integer -1234567890)] (con integer -1234567890)])",
      true
    )
    check("(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con integer 2)])", false)
    {
      val code = "(program 1.0.0 [[(builtin equalsInteger) (con bool True)] (con integer 2)])"
      assertThrows[Exception] { eval(code) }
    }

    {
      val code = "(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con bool True)])"
      assertThrows[Exception](eval(code))
    }

  }
