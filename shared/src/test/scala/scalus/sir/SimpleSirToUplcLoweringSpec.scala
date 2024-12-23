package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.*
import scalus.sir.Recursivity.NonRec
import scalus.sir.SIRType.{FreeUnificator, SumCaseClass, TypeVar}
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term
import scalus.uplc.TermDSL.{*, given}

import scala.language.implicitConversions

class SimpleSirToUplcLoweringSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:
    extension (s: SIR)
        infix def lowersTo(r: Term): Unit =
            assert(s.toUplc() == r)

    test("lower constant") {
        forAll { (c: Constant) =>
            SIR.Const(c, SIRType.IntegerPrimitive) lowersTo Term.Const(c)
        }
    }

    test("lower error") {
        SIR.Error("error") lowersTo Term.Error
        assert(
          SIR.Error("error")
              .toUplc(generateErrorTraces = true) == !(!Trace $ "error" $ ~(Term.Error))
        )
    }

    test("lower Var") { SIR.Var("x", SIRType.ByteStringPrimitive) lowersTo vr"x" }

    test("lower Lam/Apply") {
        import SIRType.{TypeLambda, TypeVar, VoidPrimitive}
        val idType = TypeLambda(List(TypeVar("A", Some(1))), TypeVar("A", Some(1)))
        val x = SIR.Var("x", TypeVar("X", Some(2)))
        SIR.Apply(
          SIR.LamAbs(x, x),
          SIR.Const(Constant.Unit, VoidPrimitive),
          VoidPrimitive
        ) lowersTo (lam("x")(vr"x") $ Constant.Unit)

    }

    test("lower builtins") {
        SIRBuiltins.addInteger lowersTo AddInteger
        SIRBuiltins.headList lowersTo !HeadList
        SIRBuiltins.fstPair lowersTo !(!FstPair)
    }

    test("lower let") {
        import SIRType.{Fun, IntegerPrimitive}
        /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
         */
        SIR.Let(
          NonRec,
          Binding("x", SIR.Const(asConstant(1), IntegerPrimitive)) :: Binding(
            "y",
            SIR.Const(asConstant(2), IntegerPrimitive)
          ) :: Nil,
          SIR.Apply(
            SIR.Apply(
              SIRBuiltins.addInteger,
              SIR.Var("x", IntegerPrimitive),
              Fun(IntegerPrimitive, IntegerPrimitive)
            ),
            SIR.Var("y", IntegerPrimitive),
            IntegerPrimitive
          )
        ) lowersTo (lam("x")(lam("y")(AddInteger $ vr"x" $ vr"y") $ 2) $ 1)
    }

    test("lower Constr") {
        import SIRType.{Fun, TypeVar, TypeProxy, IntegerPrimitive, ByteStringPrimitive}
        import SIRVarStorage.DEFAULT
        /* Nil
       lowers to (\Nil Cons -> force Nil)
       TxId(name)
       lowers to (\name TxId -> TxId name) name
         */
        val a1TypeVar = TypeVar("A", Some(1))
        val a2TypeVar = TypeVar("A", Some(2))
        val tailTypeProxy = new TypeProxy(null)
        val listData =
            DataDecl(
              "List",
              List(
                ConstrDecl("Nil", DEFAULT, List(), List()),
                ConstrDecl(
                  "Cons",
                  DEFAULT,
                  List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
                  List(a2TypeVar)
                )
              ),
              List(a1TypeVar)
            )
        tailTypeProxy.ref = SumCaseClass(listData, List(a2TypeVar))
        val txIdData = DataDecl(
          "TxId",
          List(ConstrDecl("TxId", DEFAULT, List(TypeBinding("hash", ByteStringPrimitive)), List())),
          List()
        )
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        withDecls(SIR.Constr("Nil", listData, List())) lowersTo (lam("Nil", "Cons")(
          !(vr"Nil")
        ))
        withDecls(
          SIR.Constr(
            "TxId",
            txIdData,
            List(SIR.Const(asConstant(hex"DEADBEEF"), ByteStringPrimitive))
          )
        ) lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        val a = SIR.Var("a", SIRType.BooleanPrimitive)
        val b = SIR.Var("b", SIRType.BooleanPrimitive)
        SIR.And(a, b) lowersTo !(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)
        SIR.Or(a, b) lowersTo !(!IfThenElse $ vr"a" $ ~true $ ~vr"b")
        SIR.Not(a) lowersTo !(!IfThenElse $ vr"a" $ ~false $ ~true)
    }

    test("lower Match") {
        /* Nil match
        case Nil -> 1
        case Cons(h, tl) -> 2

       lowers to (\Nil Cons -> force Nil) (delay 1) (\h tl -> 2)
         */
        val tailTypeProxy = new SIRType.TypeProxy(null)
        val a1TypeVar = SIRType.TypeVar("A1", Some(1))
        val a2TypeVar = SIRType.TypeVar("A2", Some(2))
        val nilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, List(), List())
        val consConstr = ConstrDecl(
          "Cons",
          SIRVarStorage.DEFAULT,
          List(TypeBinding("head", a2TypeVar), TypeBinding("tail", tailTypeProxy)),
          List(a2TypeVar)
        )
        val listData = DataDecl("List", List(nilConstr, consConstr), List(a1TypeVar))
        tailTypeProxy.ref = SumCaseClass(listData, List(a2TypeVar))

        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl(
              "TxId",
              SIRVarStorage.DEFAULT,
              List(TypeBinding("hash", SIRType.ByteStringPrimitive)),
              List()
            )
          ),
          List()
        )

        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))

        val listAnyType = SIRType.SumCaseClass(listData, List(FreeUnificator))

        withDecls(
          SIR.Match(
            SIR.Constr("Nil", listData, List()),
            List(
              SIR.Case(
                nilConstr,
                Nil,
                Nil,
                SIR.Const(Constant.Integer(1), SIRType.IntegerPrimitive)
              ),
              SIR.Case(
                consConstr,
                List("h", "tl"),
                List(SIRType.FreeUnificator, listAnyType),
                SIR.Const(Constant.Integer(2), SIRType.IntegerPrimitive)
              )
            ),
            SIRType.IntegerPrimitive
          )
        ) lowersTo (lam("Nil", "Cons")(!vr"Nil") $ ~asConstant(1) $ lam("h", "tl")(2))
    }
