package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.StringInterpolators
import scalus.sir.Recursivity.NonRec
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term
import scalus.uplc.TermDSL.{_, given}

class SimpleSirToUplcLoweringSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:
    extension (s: SIR)
        infix def lowersTo(r: Term): Unit =
            assert(s.toUplc() == r)

    test("lower constant") {
        forAll { (c: Constant) =>
            SIR.Const(c) lowersTo Term.Const(c)
        }
    }

    test("lower error") {
        SIR.Error("error") lowersTo Term.Error
        assert(
          SIR.Error("error")
              .toUplc(generateErrorTraces = true) == !(!Trace $ "error" $ ~(Term.Error))
        )
    }

    test("lower Var") { SIR.Var("x") lowersTo vr"x" }

    test("lower Lam/Apply") {
        SIR.Apply(
          SIR.LamAbs("x", SIR.Var("x")),
          SIR.Const(Constant.Unit)
        ) lowersTo (lam("x")(vr"x") $ Constant.Unit)

    }

    test("lower builtins") {
        SIR.Builtin(AddInteger) lowersTo AddInteger
        SIR.Builtin(HeadList) lowersTo !HeadList
        SIR.Builtin(FstPair) lowersTo !(!FstPair)
    }

    test("lower let") {
        /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
         */
        SIR.Let(
          NonRec,
          Binding("x", SIR.Const(asConstant(1))) :: Binding("y", SIR.Const(asConstant(2))) :: Nil,
          SIR.Apply(
            SIR.Apply(SIR.Builtin(AddInteger), SIR.Var("x")),
            SIR.Var("y")
          )
        ) lowersTo (lam("x")(lam("y")(AddInteger $ vr"x" $ vr"y") $ 2) $ 1)
    }

    test("lower Constr") {
        /* Nil
       lowers to (\Nil Cons -> force Nil)
       TxId(name)
       lowers to (\name TxId -> TxId name) name
         */
        val listData =
            DataDecl(
              "List",
              List(ConstrDecl("Nil", List()), ConstrDecl("Cons", List("head", "tail")))
            )
        val txIdData = DataDecl("TxId", List(ConstrDecl("TxId", List("hash"))))
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        withDecls(SIR.Constr("Nil", listData, List())) lowersTo (lam("Nil", "Cons")(
          !(vr"Nil")
        ))
        withDecls(
          SIR.Constr("TxId", txIdData, List(SIR.Const(asConstant(hex"DEADBEEF"))))
        ) lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        SIR.And(SIR.Var("a"), SIR.Var("b")) lowersTo !(!IfThenElse $ vr"a" $ ~vr"b" $ ~false)
        SIR.Or(SIR.Var("a"), SIR.Var("b")) lowersTo !(!IfThenElse $ vr"a" $ ~true $ ~vr"b")
        SIR.Not(SIR.Var("a")) lowersTo !(!IfThenElse $ vr"a" $ ~false $ ~true)
    }

    test("lower Match") {
        /* Nil match
        case Nil -> 1
        case Cons(h, tl) -> 2

       lowers to (\Nil Cons -> force Nil) (delay 1) (\h tl -> 2)
         */
        val nilConstr = ConstrDecl("Nil", List())
        val consConstr = ConstrDecl("Cons", List("head", "tail"))
        val listData =
            DataDecl("List", List(nilConstr, consConstr))
        val txIdData = DataDecl("TxId", List(ConstrDecl("TxId", List("hash"))))
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        withDecls(
          SIR.Match(
            SIR.Constr("Nil", listData, List()),
            List(
              Case(nilConstr, Nil, SIR.Const(Constant.Integer(1))),
              Case(consConstr, List("h", "tl"), SIR.Const(Constant.Integer(2)))
            )
          )
        ) lowersTo (lam("Nil", "Cons")(!vr"Nil") $ ~asConstant(1) $ lam("h", "tl")(2))
    }
