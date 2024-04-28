package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.uplc.ArbitraryInstances
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}

import EtaReduce.etaReduce

class EtaReduceSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
    test("(lam x [f x]) reduces to f"):
        assert(etaReduce(λ("x")(vr"f" $ vr"x")) == vr"f")

    test("(lam x [(builtin unBData) x]) reduces to (builtin unBData)"):
        assert(etaReduce(λ("x")(Builtin(UnBData) $ vr"x")) == Builtin(UnBData))

    test("(lam x [(lam f f) x]) reduces to (lam f f)"):
        assert(etaReduce(λ("x")(λ("f")(vr"f") $ vr"x")) == λ("f")(vr"f"))

    test("(lam x (lam y [f x y])) does not reduce as [f x] may have side effects"):
        assert(etaReduce(λ("x", "y")(vr"f" $ vr"x" $ vr"y")) == λ("x", "y")(vr"f" $ vr"x" $ vr"y"))

    test("(lam x [(error) x]) does not reduce"):
        assert(etaReduce(λ("x")(Error $ vr"x")) == λ("x")(Error $ vr"x"))

    test("(lam x [(force f) x]) does not reduce"):
        assert(etaReduce(λ("x")(Force(vr"f") $ vr"x")) == λ("x")(Force(vr"f") $ vr"x"))

    test("(lam x [(builtin divideInteger) 1 x]) does not reduce"):
        assert(
          etaReduce(λ("x")(Builtin(DivideInteger) $ 1 $ vr"x")) == λ("x")(
            Builtin(DivideInteger) $ 1 $ vr"x"
          )
        )

    test("(lam x [x x]) does not reduce as x is bound in the function body"):
        assert(etaReduce(λ("x")(vr"x" $ vr"x")) == λ("x")(vr"x" $ vr"x"))
