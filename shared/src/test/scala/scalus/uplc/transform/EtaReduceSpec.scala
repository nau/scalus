package scalus.uplc.transform

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.transform.EtaReduce.etaReduce

import scala.language.implicitConversions

class EtaReduceSpec extends AnyFunSuite:
    test("(lam x [f x]) reduces to f"):
        assert(etaReduce(λ("x")(x => vr"f" $ x)) == vr"f")

    test("(lam x [(builtin unBData) x]) reduces to (builtin unBData)"):
        assert(etaReduce(λ("x")(x => Builtin(UnBData) $ x)) == Builtin(UnBData))

    test("(lam x [(lam f f) x]) reduces to (lam f f)"):
        assert(etaReduce(λ("x")(x => λ("f")(f => f) $ x)) == λ("f")(f => f))

    test("(lam x (lam y [f x y])) does not reduce as [f x] may have side effects"):
        assert(
          etaReduce(λ("x", "y")((x, y) => vr"f" $ x $ y)) == λ("x", "y")((x, y) => vr"f" $ x $ y)
        )

    test("(lam x [(error) x]) does not reduce"):
        assert(etaReduce(λ("x")(x => Error $ x)) == λ("x")(x => Error $ x))

    test("(lam x [(delay error) x]) reduces to (delay error)"):
        assert(etaReduce(λ("x")(x => Delay(Error) $ x)) == Delay(Error))

    test("(lam x [(force f) x]) does not reduce"):
        assert(etaReduce(λ("x")(x => Force(vr"f") $ x)) == λ("x")(x => Force(vr"f") $ x))

    test("(lam x [(builtin divideInteger) 1 x]) does not reduce"):
        assert(
          etaReduce(λ("x")(x => Builtin(DivideInteger) $ 1 $ x)) == λ("x")(x =>
              Builtin(DivideInteger) $ 1 $ x
          )
        )

    test("(lam x [x x]) does not reduce as x is bound in the function body"):
        assert(etaReduce(λ("x")(x => x $ x)) == λ("x")(x => x $ x))
