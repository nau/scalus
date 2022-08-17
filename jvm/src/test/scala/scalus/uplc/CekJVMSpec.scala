package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.uplc.Constant.Pair
import scalus.uplc.DefaultFun.{AddInteger, EqualsInteger, UnConstrData}
import scalus.uplc.DefaultUni.{Bool, ByteString, asConstant}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
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

  test("UnMapData") {
    assert(
      Cek.evalUPLC(DefaultFun.UnMapData $ Data.Map((12, 1) :: Nil)) == Const(
        Constant.List(
          DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
          Pair(Constant.Data(12), Constant.Data(1)) :: Nil
        )
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.Map(elems) =>
          val result = Cek.evalUPLC(DefaultFun.UnMapData $ t)
          assert(
            result == Const(
              Constant.List(
                DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data),
                elems.map { case (k, v) =>
                  Pair(Constant.Data(k), Constant.Data(v))
                }
              )
            )
          )
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnMapData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnMapData $ t)
      )
    }
  }

  test("UnListData") {
    assert(
      Cek.evalUPLC(DefaultFun.UnListData $ Data.List(Data.I(12) :: Data.I(1) :: Nil)) == Const(
        Constant.List(
          DefaultUni.Data,
          Constant.Data(12) :: Constant.Data(1) :: Nil
        )
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.List(elems) =>
          val result = Cek.evalUPLC(DefaultFun.UnListData $ t)
          assert(
            result == Const(
              Constant.List(
                DefaultUni.Data,
                elems.map(Constant.Data.apply)
              )
            )
          )
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnListData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnListData $ t)
      )
    }
  }

  test("UnIData") {
    assert(Cek.evalUPLC(DefaultFun.UnIData $ Data.I(12)) == Const(Constant.Integer(12)))

    forAll { (t: Data) =>
      t match
        case Data.I(v) =>
          val result = Cek.evalUPLC(DefaultFun.UnIData $ t)
          assert(result == Const(Constant.Integer(v)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnIData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnIData $ t)
      )
    }
  }

  test("UnBData") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(DefaultFun.UnBData $ Data.B(hex"deadbeef")) == Const(
        Constant.ByteString(hex"deadbeef")
      )
    )

    forAll { (t: Data) =>
      t match
        case Data.B(v) =>
          val result = Cek.evalUPLC(DefaultFun.UnBData $ t)
          assert(result == Const(Constant.ByteString(v)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.UnBData $ t))
    }

    // FIXME: now Arbitrary[Term] doesn't generate Data. When it does, update this test
    forAll { (t: Term) =>
      assertThrows[Exception](
        Cek.evalUPLC(DefaultFun.UnBData $ t)
      )
    }
  }

  test("NullList") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(!DefaultFun.NullList $ Const(Constant.List(DefaultUni.Integer, Nil))) == Const(
        asConstant(true)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(_, v) =>
          val result = Cek.evalUPLC(!DefaultFun.NullList $ t)
          assert(result == Const(Constant.Bool(v.isEmpty)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.NullList $ t))
    }
  }

  test("HeadList") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !DefaultFun.HeadList $ Const(Constant.List(DefaultUni.Integer, asConstant(1) :: Nil))
      ) == Const(
        asConstant(1)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(_, v) if v.nonEmpty =>
          val result = Cek.evalUPLC(!DefaultFun.HeadList $ t)
          assert(result == Const(v.head))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.HeadList $ t))
    }
  }

  test("TailList") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !DefaultFun.TailList $ Const(
          Constant.List(DefaultUni.Integer, asConstant(1) :: asConstant(2) :: Nil)
        )
      ) == Const(
        Constant.List(DefaultUni.Integer, asConstant(2) :: Nil)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.List(tpe, v) if v.nonEmpty =>
          val result = Cek.evalUPLC(!DefaultFun.TailList $ t)
          assert(result == Const(Constant.List(tpe, v.tail)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!DefaultFun.TailList $ t))
    }
  }

  test("FstPair") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !(!DefaultFun.FstPair) $ Const(Constant.Pair(asConstant(1), asConstant(false)))
      ) == Const(
        asConstant(1)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.Pair(a, _) =>
          val result = Cek.evalUPLC(!(!DefaultFun.FstPair) $ t)
          assert(result == Const(a))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!(!DefaultFun.FstPair) $ t))
    }
  }

  test("SndPair") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        !(!DefaultFun.SndPair) $ Const(Constant.Pair(asConstant(1), asConstant(false)))
      ) == Const(
        asConstant(false)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.Pair(a, _) =>
          val result = Cek.evalUPLC(!(!DefaultFun.SndPair) $ t)
          assert(result == Const(a))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(!(!DefaultFun.SndPair) $ t))
    }
  }

  test("EqualsByteString") {
    import scalus.utils.Utils.*
    assert(
      Cek.evalUPLC(
        DefaultFun.EqualsByteString $ Const(Constant.ByteString(hex"deadbeef")) $ Const(
          Constant.ByteString(hex"deadbeef")
        )
      ) == Const(
        asConstant(true)
      )
    )
    assert(
      Cek.evalUPLC(
        DefaultFun.EqualsByteString $ Const(Constant.ByteString(hex"")) $ Const(
          Constant.ByteString(hex"deadbeef")
        )
      ) == Const(
        asConstant(false)
      )
    )

    forAll { (t: Constant) =>
      t match
        case Constant.ByteString(_) =>
          val result = Cek.evalUPLC(DefaultFun.EqualsByteString $ t $ t)
          assert(result == Const(asConstant(true)))
        case _ =>
          assertThrows[Exception](Cek.evalUPLC(DefaultFun.EqualsByteString $ t $ t))
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

  test("simple validator example") {
    import TermDSL.*
    import scalus.utils.Utils.*

    // simple validator that checks that the spending transaction has no outputs
    // it's a gift to the validators community
    val validator = λ("redeemer", "datum", "ctx") {
      // ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
      val scriptContext = DefaultFun.UnConstrData $ Var("ctx")
      // ScriptContext args
      val ctxArgs = !(!DefaultFun.SndPair) $ scriptContext
      // second in the list
      val txInfo = DefaultFun.UnConstrData $ (!DefaultFun.HeadList $ ctxArgs)
      val txInfoArgs = !(!DefaultFun.SndPair) $ txInfo
      val txInfoOutputs =
        !DefaultFun.HeadList $ (!DefaultFun.TailList $ (!DefaultFun.TailList $ txInfoArgs))
      val isTxInfoOutputsEmpty = !DefaultFun.NullList $ txInfoOutputs
      val result = !(!DefaultFun.IfThenElse $ isTxInfoOutputsEmpty $ ~() $ ~Error)
      result
    }
    println(validator.pretty.render(80))
    println(Cek.evalUPLC(validator).pretty.render(80))
    val program = Program((1, 0, 0), validator).pretty.render(80)

    import scala.sys.process.*
    val cmd = "/Users/nau/projects/scalus/uplc convert --of flat"
    val outStream = new ByteArrayOutputStream()
    val out =
      cmd.#<(new ByteArrayInputStream(program.getBytes("UTF-8"))).#>(outStream).!
    val bytes = outStream.toByteArray
    println(s"${bytes.length} bytes: ${bytesToHex(bytes)}")

  }
