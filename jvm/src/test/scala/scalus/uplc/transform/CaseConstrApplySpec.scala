package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.{ByteString, given}
import scalus.uplc.Constant
import scalus.uplc.Term.*
import scalus.uplc.eval.ExBudget.given
import scalus.uplc.eval.Result.Success
import scalus.uplc.eval.{ExBudget, PlutusVM}

import scala.language.implicitConversions
import scala.math.Ordering.Implicits.*

class CaseConstrApplySpec extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("replace (apply (apply (apply f a) b) c) with (case (constr 0 [a, b, c]) f)") {
        val sir = compile(((a: BigInt) => (b: BigInt) => (c: ByteString) => c)(0)(1)(hex"1012"))
        val uplc = sir.toUplc()
        val (optimized, logs) = CaseConstrApply.extractPass(uplc)
        assert(
          optimized == Case(
            Constr(
              0,
              List(
                Const(Constant.Integer(0)),
                Const(Constant.Integer(1)),
                Const(Constant.ByteString(hex"1012"))
              )
            ),
            λ("a", "b", "c")(vr"c") :: Nil
          )
        )
        assert(logs == Seq("Replacing 3 Apply with Case/Constr"))
        (uplc.evaluateDebug, optimized.evaluateDebug) match
            case (orig: Success, opt: Success) =>
                assert(orig.term == Const(Constant.ByteString(hex"1012")))
                assert(opt.term == Const(Constant.ByteString(hex"1012")))
                assert(orig.budget == ExBudget.fromCpuAndMemory(160100, 1100))
                assert(opt.budget == ExBudget.fromCpuAndMemory(144100, 1000))
                assert(opt.budget < orig.budget)
            case _ => fail("Evaluation failed")
    }
}
