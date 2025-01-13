package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.given
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.*
import scalus.uplc.eval.PlutusVM

import scala.language.implicitConversions

class ApplyAsConstrCaseSpec extends AnyFunSuite {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("replace (apply (apply (apply f a) b) c) with (case (constr 0 [a, b, c]) f)") {
        val sir = compile(sliceByteString(0, 1, hex"1012"))
        val uplc = sir.toUplc()
        val (optimized, logs) = ApplyAsConstrCase.extractPass(uplc)
        println(logs.mkString("\n"))
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
            Builtin(DefaultFun.SliceByteString) :: Nil
          )
        )
        assert(uplc.evaluate == Const(Constant.ByteString(hex"10")))
        assert(optimized.evaluate == Const(Constant.ByteString(hex"10")))
    }
}
