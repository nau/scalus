package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.{AddInteger, EqualsInteger, UnConstrData}
import scalus.uplc.DefaultUni.{Bool, ByteString, asConstant}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}

import java.io.ByteArrayInputStream
import scala.io.Source.fromFile

class CekJVMSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
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
    UplcParser.term
      .parse(out)
      .map(_._2)
      .getOrElse(
        throw new Exception(
          s"Could not parse: $out"
        )
      )
  }

  def run(code: String) = {
    val parser = UplcParser
    for
      program <- parser.parseProgram(code)
      evaled = Cek.evalUPLCProgram(program)
    do println(evaled.pretty.render(80))
  }

  def eval(code: String): Term = {
    val parser = UplcParser
    parser.parseProgram(code).map(Cek.evalUPLCProgram).getOrElse(sys.error("Parse error"))
  }

  test("AddInteger") {
    forAll { (a: BigInt, b: BigInt) =>
      Cek.evalUPLC(AddInteger $ a $ b) match
        case Const(Constant.Integer(r)) => assert(r == (a + b))
        case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")
    }

    forAll { (a: Term, b: Term) =>
      (a, b) match
        case (Const(Constant.Integer(aa)), Const(Constant.Integer(bb))) =>
          val r = aa + bb
          assert(Cek.evalUPLC(AddInteger $ a $ b) == Const(Constant.Integer(r)))
        case _ => assertThrows[Exception](Cek.evalUPLC(AddInteger $ a $ b))
    }
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

    forAll { (a: BigInt, b: BigInt) =>
      Cek.evalUPLC(EqualsInteger $ a $ a) match
        case Const(Constant.Bool(true)) => assert(true)
        case r                          => fail(s"Expected true but got ${r.pretty.render(80)}")

      Cek.evalUPLC(EqualsInteger $ a $ b) match
        case Const(Constant.Bool(r)) => assert(r == (a == b))
        case r                       => fail(s"Expected true but got ${r.pretty.render(80)}")
    }
  }

  test("UnConstrData") {
    assert(
      Cek.evalUPLC(DefaultFun.UnConstrData $ Data.Constr(12, 1 :: Nil)) == Const(
        Pair(asConstant(12), Constant.List(DefaultUni.Data, List(Constant.Data(Data.I(1)))))
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.Constr(constr, args) =>
          val result = Cek.evalUPLC(DefaultFun.UnConstrData $ t)
          assert(
            result == Const(
              Pair(asConstant(constr), Constant.List(DefaultUni.Data, args.map(asConstant)))
            )
          )
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnConstrData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnConstrData $ t)
      )
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

    // Examples
    check("example/factorial/factorial")
    check("example/fibonacci/fibonacci")
  }
