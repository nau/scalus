package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.ledger.api.v3.TxId
import scalus.sir.Recursivity.NonRec
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.{Constant, DefaultFun, Term}
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions

class SirToUplc110LoweringTest extends AnyFunSuite, ScalaCheckPropertyChecks, ArbitraryInstances {
    extension (sir: SIR)
        infix def lowersTo(r: Term): Unit =
            assert(SirToUplc110Lowering(sir, generateErrorTraces = false).lower() == r)

    private val ae = AnnotationsDecl.empty

    test("lower constant") {
        forAll { (c: Constant) =>
            SIR.Const(c, SIRType.Integer, ae) lowersTo Term.Const(c)
        }
    }

    test("lower error") {
        SIR.Error("error", ae) lowersTo Term.Error
        assert(
          SIR.Error("error", ae)
              .toUplc(generateErrorTraces = true) == !(!Trace $ "error" $ ~Term.Error)
        )
    }

    test("lower Var") {
        SIR.Var("x", SIRType.ByteString, ae) lowersTo vr"x"
    }

    test("lower Lam/Apply") {
        import SIRType.{TypeLambda, TypeVar, Unit}
        val idType = TypeLambda(List(TypeVar("A", Some(1))), TypeVar("A", Some(1)))
        val x = SIR.Var("x", TypeVar("X", Some(2)), ae)
        SIR.Apply(
          SIR.LamAbs(x, x, ae),
          SIR.Const(Constant.Unit, Unit, ae),
          Unit,
          ae
        ) lowersTo (lam("x")(vr"x") $ Constant.Unit)

    }

    test("lower builtins") {
        SIRBuiltins.addInteger lowersTo AddInteger
        SIRBuiltins.headList lowersTo !HeadList
        SIRBuiltins.fstPair lowersTo !(!FstPair)
    }

    test("lower let") {
        import SIRType.{Fun, Integer}
        /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
         */
        SIR.Let(
          NonRec,
          Binding("x", Integer, SIR.Const(asConstant(1), Integer, ae)) :: Binding(
            "y",
            Integer,
            SIR.Const(asConstant(2), Integer, ae)
          ) :: Nil,
          SIR.Apply(
            SIR.Apply(
              SIRBuiltins.addInteger,
              SIR.Var("x", Integer, ae),
              Fun(Integer, Integer),
              ae
            ),
            SIR.Var("y", Integer, ae),
            Integer,
            ae
          ),
          ae
        ) lowersTo (lam("x")(lam("y")(AddInteger $ vr"x" $ vr"y") $ 2) $ 1)
    }

    test("lower Constr") {
        val sir = compile { prelude.List.Nil: prelude.List[BigInt] }
        sir lowersTo Term.Constr(0, List.empty)
    }

    test("lower newtype Constr") {
        val sir = compile { TxId(hex"DEADBEEF") }
        sir lowersTo Term.Const(Constant.ByteString(hex"DEADBEEF"))
    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        val a = SIR.Var("a", SIRType.Boolean, ae)
        val b = SIR.Var("b", SIRType.Boolean, ae)
        SIR.And(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)
        SIR.Or(a, b, ae) lowersTo !(!IfThenElse $ vr"a" $ ~true $ ~vr"b")
        SIR.Not(a, ae) lowersTo !(!IfThenElse $ vr"a" $ ~false $ ~true)
    }

    test("lower Match") {
        /* list match
            case Nil -> error
            case Cons(h, tl) -> 2

            lowers to (case list [error, \h tl -> 2])

            newtype match
                case Newtype(a) -> error

            lowers to (\a -> error) newtype
         */

        val sir = compile {
            (prelude.List.Nil: prelude.List[BigInt]) match
                case prelude.List.Nil         => BigInt(1)
                case prelude.List.Cons(h, tl) => BigInt(2)
        }

        sir lowersTo Term.Case(Term.Constr(0, List.empty), List(BigInt(1), λ("h", "tl")(BigInt(2))))
    }

    test("lower newtype Match") {
        /* list match
            case Nil -> error
            case Cons(h, tl) -> 2

            lowers to (case list [error, \h tl -> 2])

            newtype match
                case Newtype(a) -> error

            lowers to (\a -> error) newtype
         */
        val sir = compile {
            TxId(hex"DEADBEEF") match
                case TxId(id) => BigInt(1)
        }

        // TODO: we can optimize this to just hex"DEADBEEF"
        sir lowersTo (lam("id")(BigInt(1)) $ hex"DEADBEEF")
    }

    test("lower newtype Match with wildcard pattern") {
        val sir = compile {
            TxId(hex"DEADBEEF") match
                case _ => BigInt(1)
        }

        sir lowersTo BigInt(1)
    }

    test("lower Select") {
        /*  x.field2
            lowers to
            (case x [\f1 f2 ... -> f2])

            newtype.field1
            lowers to
            newtype
         */
        val sir = compile {
            (hex"DEADBEEF", true)._2
        }
        sir lowersTo Term.Case(
          Term.Constr(0, List(hex"DEADBEEF", true)),
          List(λ("_1", "_2")(vr"_2"))
        )
    }

    test("lower newtype Select") {
        /*  x.field2
            lowers to
            (case x [\f1 f2 ... -> f2])

            newtype.field1
            lowers to
            newtype
         */
        val sir = compile {
            TxId(hex"DEADBEEF").hash
        }
        sir lowersTo Term.Const(Constant.ByteString(hex"DEADBEEF"))
    }

}
