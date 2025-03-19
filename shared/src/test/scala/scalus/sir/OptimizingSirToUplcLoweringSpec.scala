package scalus.sir
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.sir.Recursivity.NonRec
import scalus.sir.SIR.Pattern
import scalus.sir.SIRType.{ByteString, TypeVar}
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term
import scalus.uplc.TermDSL.{*, given}

import scala.language.implicitConversions
import scalus.uplc.NamedDeBruijn

class OptimizingSirToUplcLoweringSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:
    extension (s: SIR)
        infix def lowersTo(r: Term): Unit =
            val uplc = OptimizingSirToUplcLowering(s, forceBuiltins = ForceBuiltins.None).lower()
            assert(s.toUplc() == r)

    val sirInt = SIRType.Integer

    test("lower constant") {
        forAll { (c: Constant) =>
            SIR.Const(c, SIRType.fromDefaultUni(c.tpe), AnnotationsDecl.empty) lowersTo Term.Const(
              c
            )
        }
    }

    test("lower error") {
        SIR.Error("error", AnnotationsDecl.empty) lowersTo Term.Error
        assert(
          SIR.Error("error", AnnotationsDecl.empty)
              .toUplc(generateErrorTraces = true) == !(!Trace $ "error" $ ~(Term.Error))
        )
    }

    test("lower Var") { SIR.Var("x", SIRType.FreeUnificator, AnnotationsDecl.empty) lowersTo vr"x" }

    test("lower Lam/Apply") {
        val ae = AnnotationsDecl.empty
        val xTypeVar = SIRType.TypeVar("X", Some(1))
        val xVar = SIR.Var("x", SIRType.TypeLambda(List(xTypeVar), xTypeVar), ae)
        SIR.Apply(
          SIR.LamAbs(xVar, xVar, ae),
          SIR.Const(Constant.Unit, SIRType.Unit, ae),
          SIRType.Unit,
          ae
        ) lowersTo (lam("x")(vr"x") $ Constant.Unit)

    }

    test("lower builtins") {
        SIRBuiltins.addInteger lowersTo AddInteger
        SIRBuiltins.headList lowersTo !HeadList
        SIRBuiltins.fstPair lowersTo !(!FstPair)
    }

    test("lower let") {
        /* let x = 1 in
       let y = 2 in x + y
       lowers to (\x -> (\y -> x + y) 2) 1
         */
        val ae = AnnotationsDecl.empty
        SIR.Let(
          NonRec,
          Binding("x", SIR.Const(asConstant(1), sirInt, ae)) :: Binding(
            "y",
            SIR.Const(asConstant(2), sirInt, ae)
          ) :: Nil,
          SIR.Apply(
            SIR.Apply(
              SIRBuiltins.addInteger,
              SIR.Var("x", sirInt, ae),
              SIRType.Fun(sirInt, sirInt),
              ae
            ),
            SIR.Var("y", sirInt, ae),
            sirInt,
            ae
          ),
          ae
        ) lowersTo (lam("x")(lam("y")(AddInteger $ vr"x" $ vr"y") $ 2) $ 1)
    }

    test("lower Constr") {
        /* Nil
       lowers to (\Nil Cons -> force Nil)
       TxId(name)
       lowers to (\name TxId -> TxId name) name
         */
        val ae = AnnotationsDecl.empty
        val listDataDeclTypeProxy = new SIRType.TypeProxy(null)
        val listData =
            DataDecl(
              "List",
              List(
                ConstrDecl("Nil", SIRVarStorage.DEFAULT, List(), List(), List(), ae),
                ConstrDecl(
                  "Cons",
                  SIRVarStorage.DEFAULT,
                  List(
                    TypeBinding("head", TypeVar("T", Some(1))),
                    TypeBinding("tail", listDataDeclTypeProxy)
                  ),
                  List(TypeVar("T", Some(1))),
                  List(TypeVar("T", Some(1))),
                  ae
                )
              ),
              List(TypeVar("T", Some(1))),
              ae
            )
        listDataDeclTypeProxy.ref = SIRType.SumCaseClass(listData, List(TypeVar("T", Some(1))))
        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl(
              "TxId",
              SIRVarStorage.DEFAULT,
              List(TypeBinding("hash", ByteString)),
              List(),
              List(),
              ae
            )
          ),
          List(),
          ae
        )

        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))

        withDecls(
          SIR.Constr("Nil", listData, List(), listData.constrType("Nil"), ae)
        ) lowersTo (lam(
          "Nil",
          "Cons"
        )(
          !(vr"Nil")
        ))
        withDecls(
          SIR.Constr(
            "TxId",
            txIdData,
            List(SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae)),
            txIdData.constrType("TxId"),
            ae
          )
        ) lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        val ae = AnnotationsDecl.empty
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

       lowers to (\Nil Cons -> force Nil) (delay 1) (\h tl -> 2)
         */
        val ae = AnnotationsDecl.empty
        val listTp = SIRType.TypeProxy(null)
        val listTpX = SIRType.TypeProxy(null)
        val nilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, List(), List(), List(), ae)
        val consConstr = ConstrDecl(
          "Cons",
          SIRVarStorage.DEFAULT,
          List(TypeBinding("head", TypeVar("x", Some(1))), TypeBinding("tail", listTpX)),
          List(TypeVar("x", Some(1))),
          List(TypeVar("x", Some(1))),
          ae
        )
        val listData =
            DataDecl("List", List(nilConstr, consConstr), List(TypeVar("T", Some(2))), ae)
        listTp.ref = listData.tp
        listTpX.ref = SIRType.SumCaseClass(listData, List(TypeVar("x", Some(1))))

        val txIdData = DataDecl(
          "TxId",
          List(
            ConstrDecl(
              "TxId",
              SIRVarStorage.DEFAULT,
              List(TypeBinding("hash", ByteString)),
              List(),
              List(),
              ae
            )
          ),
          List(),
          ae
        )
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        withDecls(
          SIR.Match(
            SIR.Constr("Nil", listData, List(), listData.constrType("Nil"), ae),
            List(
              SIR.Case(
                Pattern.Constr(nilConstr, Nil, Nil),
                SIR.Const(Constant.Integer(1), SIRType.Integer, ae)
              ),
              SIR.Case(
                Pattern.Constr(
                  consConstr,
                  List("h", "tl"),
                  List(
                    SIRType.FreeUnificator,
                    SIRType.SumCaseClass(listData, List(SIRType.FreeUnificator))
                  )
                ),
                SIR.Const(Constant.Integer(2), SIRType.Integer, ae)
              )
            ),
            SIRType.Integer,
            ae
          )
        ) lowersTo (lam("Nil", "Cons")(!vr"Nil") $ lam("h", "tl")(2) $ ~asConstant(1))
    }

    test("abstract over all used forced polymorphic builtins") {
        val sir = Compiler.compile: (d: Data) =>
            if equalsData(d, d) then unIData(unConstrData(d).snd.head) else BigInt(2)

        val lowering = OptimizingSirToUplcLowering(sir, forceBuiltins = ForceBuiltins.AllUsed)
        val uplc = lowering.lower()

        import Term.*, Constant.Integer
        assert(
          uplc == Apply(
            LamAbs(
              "__builtin_HeadList",
              Apply(
                LamAbs(
                  "__builtin_SndPair",
                  Apply(
                    LamAbs(
                      "__builtin_IfThenElse",
                      LamAbs(
                        "d",
                        Force(
                          Apply(
                            Apply(
                              Apply(
                                Var(NamedDeBruijn("__builtin_IfThenElse")),
                                Apply(
                                  Apply(Builtin(EqualsData), Var(NamedDeBruijn("d"))),
                                  Var(NamedDeBruijn("d"))
                                )
                              ),
                              Delay(
                                Apply(
                                  Builtin(UnIData),
                                  Apply(
                                    Var(NamedDeBruijn("__builtin_HeadList")),
                                    Apply(
                                      Var(NamedDeBruijn("__builtin_SndPair")),
                                      Apply(Builtin(UnConstrData), Var(NamedDeBruijn("d")))
                                    )
                                  )
                                )
                              )
                            ),
                            Delay(Const(Integer(2)))
                          )
                        )
                      )
                    ),
                    Force(Builtin(IfThenElse))
                  )
                ),
                Force(Force(Builtin(SndPair)))
              )
            ),
            Force(Builtin(HeadList))
          )
        )
    }

    test("abstract over specified used forced polymorphic builtins") {
        val sir = Compiler.compile: (d: Data) =>
            if equalsData(d, d) then unIData(unConstrData(d).snd.head) else BigInt(2)

        val lowering =
            OptimizingSirToUplcLowering(sir, forceBuiltins = ForceBuiltins.Only(Set(SndPair)))
        val uplc = lowering.lower()

        import Term.*, Constant.Integer
        assert(
          uplc == Apply(
            LamAbs(
              "__builtin_SndPair",
              LamAbs(
                "d",
                Force(
                  Apply(
                    Apply(
                      Apply(
                        Force(Builtin(IfThenElse)),
                        Apply(
                          Apply(Builtin(EqualsData), Var(NamedDeBruijn("d"))),
                          Var(NamedDeBruijn("d"))
                        )
                      ),
                      Delay(
                        Apply(
                          Builtin(UnIData),
                          Apply(
                            Force(Builtin(HeadList)),
                            Apply(
                              Var(NamedDeBruijn("__builtin_SndPair")),
                              Apply(Builtin(UnConstrData), Var(NamedDeBruijn("d")))
                            )
                          )
                        )
                      )
                    ),
                    Delay(Const(Integer(2)))
                  )
                )
              )
            ),
            Force(Force(Builtin(SndPair)))
          )
        )
    }

    test("when generateErrorTraces = true then Error is wrapped in a trace") {
        val sir = Compiler.compile(throw new Exception("error"))

        val lowering =
            OptimizingSirToUplcLowering(
              sir,
              generateErrorTraces = true,
              forceBuiltins = ForceBuiltins.AllUsed
            )
        val uplc = lowering.lower()
        import Term.*, Constant.String
        assert(
          uplc == Apply(
            LamAbs(
              "__builtin_Trace",
              Force(
                Apply(
                  Apply(Var(NamedDeBruijn("__builtin_Trace")), Const(String("error"))),
                  Delay(Error)
                )
              )
            ),
            Force(Builtin(Trace))
          )
        )
    }
