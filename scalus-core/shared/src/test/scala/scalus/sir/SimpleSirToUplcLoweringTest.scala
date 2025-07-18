package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.*
import scalus.sir.Recursivity.NonRec
import scalus.sir.SIR.Pattern
import scalus.sir.SIRType.{FreeUnificator, SumCaseClass, TypeNothing, TypeVar}
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.test.ArbitraryInstances

import scala.language.implicitConversions

class SimpleSirToUplcLoweringTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:
    extension (sir: SIR)
        infix def lowersTo(r: Term): Unit =
            assert(SimpleSirToUplcLowering(sir, generateErrorTraces = false).lower() == r)

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
              .toUplc(
                generateErrorTraces = true,
                optimizeUplc = false
              ) == !(!Trace $ "error" $ ~Term.Error)
        )
    }

    test("lower Var in let") {
        SIR.Var("x", SIRType.ByteString, ae) lowersTo vr"x"
    }

    test("lower Lam/Apply") {
        import SIRType.{TypeLambda, TypeVar, Unit}
        val idType = TypeLambda(List(TypeVar("A", Some(1), false)), TypeVar("A", Some(1), false))
        val x = SIR.Var("x", TypeVar("X", Some(2), false), ae)
        SIR.Apply(
          SIR.LamAbs(x, x, List.empty, ae),
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
        import SIRType.{TypeVar, TypeProxy, ByteString}
        /* Nil
       lowers to (\Nil Cons -> force Nil)
       TxId(name)
       lowers to (\name TxId -> TxId name) name
         */
        val a1TypeVar = TypeVar("A", Some(1), false)
        val a2TypeVar = TypeVar("A", Some(2), false)
        val tailTypeProxy = new TypeProxy(null)
        val listData =
            DataDecl(
              "scalus.prelude.List",
              List(
                ConstrDecl(
                  "scalus.prelude.List$.Nil",
                  List(),
                  List(),
                  List(TypeNothing),
                  ae
                ),
                ConstrDecl(
                  "scalus.prelude.List$.Cons",
                  List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
                  List(a2TypeVar),
                  List(a2TypeVar),
                  ae
                )
              ),
              List(a1TypeVar),
              ae
            )
        tailTypeProxy.ref = SumCaseClass(listData, List(a2TypeVar))
        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl("TxId", List(TypeBinding("hash", ByteString)), List(), List(), ae)
          ),
          List(),
          ae
        )
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        withDecls(
          SIR.Constr(
            "scalus.prelude.List$.Nil",
            listData,
            List(),
            listData.constrType("scalus.prelude.List$.Nil"),
            ae
          )
        ) lowersTo (lam("scalus.prelude.List$.Nil", "scalus.prelude.List$.Cons")(
          !vr"scalus.prelude.List$$.Nil"
        ))
        withDecls(
          SIR.Constr(
            "TxId",
            txIdData,
            List(SIR.Const(asConstant(hex"DEADBEEF"), ByteString, ae)),
            txIdData.constrType("TxId"),
            ae
          )
        ) lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

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
        /* Nil match
        case Nil -> 1
        case Cons(h, tl) -> 2
        // constructors are sorted by their order of declaration
        //   in this tests this is (Nil, Cons) [see below]
        lowers to (\Nil Cons -> force Nil) (delay 1) (\h tl -> 2)
         */
        val tailTypeProxy = new SIRType.TypeProxy(null)
        val a1TypeVar = SIRType.TypeVar("A1", Some(1), false)
        val a2TypeVar = SIRType.TypeVar("A2", Some(2), false)
        val nilConstr = ConstrDecl("Nil", List(), List(), List(), ae)
        val consConstr = ConstrDecl(
          "Cons",
          List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
          List(a2TypeVar),
          List(a2TypeVar),
          ae
        )
        val listData = DataDecl("List", List(nilConstr, consConstr), List(a1TypeVar), ae)
        tailTypeProxy.ref = SumCaseClass(listData, List(a2TypeVar))

        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl(
              "TxId",
              List(TypeBinding("hash", SIRType.ByteString)),
              List(),
              List(),
              ae
            )
          ),
          List(),
          ae
        )

        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))

        val listAnyType = SIRType.SumCaseClass(listData, List(FreeUnificator))

        withDecls(
          SIR.Match(
            SIR.Constr("Nil", listData, List(), listData.constrType("Nil"), ae),
            List(
              SIR.Case(
                Pattern.Constr(nilConstr, Nil, Nil),
                SIR.Const(Constant.Integer(1), SIRType.Integer, ae),
                ae
              ),
              SIR.Case(
                Pattern
                    .Constr(consConstr, List("h", "tl"), List(SIRType.FreeUnificator, listAnyType)),
                SIR.Const(Constant.Integer(2), SIRType.Integer, ae),
                ae
              )
            ),
            SIRType.Integer,
            ae
          )
        ) lowersTo (lam("Nil", "Cons")(!vr"Nil") $ ~asConstant(1) $ lam("h", "tl")(2))
    }
