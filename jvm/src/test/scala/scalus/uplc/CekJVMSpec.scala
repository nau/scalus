package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import scalus.uplc.DefaultUni.{ByteString, asConstant}
import scalus.uplc.Term.*

import java.io.ByteArrayInputStream
import scala.io.Source.fromFile

class CekJVMSpec extends AnyFunSuite:
  test("Scalus") {
    val h = Const(Constant(ByteString, "Hello"))
    val id = LamAbs("x", Var("x"))
    val app = Apply(id, h)
    assert(Cek.evalUPLC(app) == h)
  }

  def runUPLC(code: String) = {
    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc evaluate"
    val out = cmd.#<(new ByteArrayInputStream(code.getBytes("UTF-8"))).!!
    println(out)
  }

  def evalUPLC(code: String): Term = {
    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc evaluate"
    val out = cmd.#<(new ByteArrayInputStream(code.getBytes("UTF-8"))).!!
    println(out)
    new UplcParser().term
      .parse(out)
      .map(_._2)
      .getOrElse(
        throw new Exception(
          s"Could not parse: $out"
        )
      )
  }

  def run(code: String) = {
    val parser = new UplcParser()
    for
      program <- parser.parseProgram(code)
      evaled = Cek.evalUPLC(program.term)
    do println(evaled.pretty.render(80))
  }

  def eval(code: String): Term = {
    val parser = new UplcParser()
    parser.parseProgram(code).map(t => Cek.evalUPLC(t.term)).getOrElse(sys.error("Parse error"))
  }

  test("EqualsInteger") {
    def check(code: String, result: Boolean) =
      assert(evalUPLC(code) == Const(asConstant(result)))
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
      assertThrows[Exception] { evalUPLC(code) }
      assertThrows[Exception] { eval(code) }
    }

    {
      val code = "(program 1.0.0 [[(builtin equalsInteger) (con integer 1)] (con bool True)])"
      assertThrows[Exception](evalUPLC(code))
      assertThrows[Exception](eval(code))
    }

  }

  test("conformance") {
    def check(name: String) =
      val path =
        s"/Users/nau/projects/iohk/plutus/plutus-conformance/test-cases/uplc/evaluation"
      val code = fromFile(s"$path/$name.uplc").mkString
      val expected = fromFile(s"$path/$name.uplc.expected").mkString
      println(eval(code).pretty.render(80))
      assert(eval(code) == eval(expected))

    check("builtin/addInteger/addInteger")
    check("builtin/addInteger-uncurried/addInteger-uncurried")
    check("builtin/equalsInteger/equalsInteger")
    check("builtin/ifThenElse/ifThenElse")
  }
