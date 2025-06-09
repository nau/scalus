package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.given
import scalus.sir.Recursivity.NonRec
import scalus.sir.SIR.Pattern
import scalus.sir.SIRType.{substitute, FreeUnificator, SumCaseClass, TypeVar}
import scalus.uplc.{ArbitraryInstances, Constant, DeBruijn, DefaultFun, DefaultUni, NamedDeBruijn, Term}
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.given
import scalus.uplc.eval.PlutusVM
import scalus.uplc.eval.Result.Success

import scala.language.implicitConversions

class SirToUplcSmLoweringTest
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances:

    extension (sir: SIR)
        infix def lowersTo(r: Term): Unit =
            // assert(SimpleSirToUplcLowering(sir, generateErrorTraces = false).lower() == r)
            assert(SirToUplcV3Lowering(sir, generateErrorTraces = false).lower().alphaEq(r))

    def lower(sir: SIR): Term =
        SirToUplcV3Lowering(sir, generateErrorTraces = false).lower()

    extension (term: Term)
        infix def alphaEq(other: Term): Boolean =
            Term.alphaEq(DeBruijn.deBruijnTerm(term), DeBruijn.deBruijnTerm(other))

    private val ae = AnnotationsDecl.empty

    given PlutusVM = PlutusVM.makePlutusV3VM()

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

    test("lower Var in let") {
        // now we can't lower var in isolation, so should use Var in some context
        val sir = SIR.Let(
          NonRec,
          List(Binding("x", SIR.Const(asConstant(hex"DEADBEEF"), SIRType.ByteString, ae))),
          SIR.Var("x", SIRType.ByteString, ae),
          ae
        )
        val uplc = lower(sir)
        val expected = LamAbs(
          "x",
          Var(NamedDeBruijn("x")),
        ) $ asConstant(hex"DEADBEEF")
        // println(s"Lowered SIR: ${uplc.pretty.render(100)}")
        // println(s"Expected UPLC: ${expected.pretty.render(100)}")
        assert(uplc alphaEq expected)
        // lowersTo vr"x"
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
          Binding("x", SIR.Const(asConstant(1), Integer, ae)) :: Binding(
            "y",
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
              "scalus.prelude.List",
              List(
                ConstrDecl("scalus.prelude.List$.Nil", DEFAULT, List(), List(), List(), ae),
                ConstrDecl(
                  "scalus.prelude.List$.Cons",
                  DEFAULT,
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
            ConstrDecl("TxId", DEFAULT, List(TypeBinding("hash", ByteString)), List(), List(), ae)
          ),
          List(),
          ae
        )
        def withDecls(sir: SIR) = SIR.Decl(listData, SIR.Decl(txIdData, sir))
        val originSir1 = withDecls(
          SIR.Constr(
            "scalus.prelude.List$.Nil",
            listData,
            List(),
            listData.constrType("scalus.prelude.List$.Nil"),
            ae
          )
        )

        val gen1 = scalus.sir.lowering.typegens.SirTypeUplcGenerator(originSir1.tp)
        val representation1 = gen1.defaultRepresentation
        println(s"gen1 = $gen1, representation1=${representation1}")
        assert(representation1 == scalus.sir.lowering.ProductCaseClassRepresentation.DataList)

        val genDataList =
            scalus.sir.lowering.typegens.SirTypeUplcGenerator(SIRType.List(SIRType.Data))
        println(
          s"genDataList=${genDataList}, defaultRepresentation=${genDataList.defaultRepresentation}"
        )

        val origin1 = lower(originSir1)
        val expected1 = Term.Builtin(DefaultFun.MkNilData) $ Term.Const(Constant.Unit)
        // println(s"Lowered SIR: ${origin1.pretty.render(100)}")
        // println(s"Expected UPLC: ${expected1.pretty.render(100)}")

        assert(origin1 alphaEq expected1)

        val result1 = origin1.evaluateDebug
        result1 match {
            case Success(term, _, _, _) =>
                val nilData = scalus.prelude.List.empty[scalus.builtin.Data].toData
                println(s"NilData=${nilData}")
                println(s"Term=${term}")
                val termValue = term match {
                    case Term.Const(Constant.List(DefaultUni.Data, value)) => value
                    case _ => fail(s"Expected a List constant, got: ${term}")
                }
                println(s"TermValue=${termValue}")
                /// assert(termValue == nilData)
                assert(term == Term.Const(Constant.List(scalus.uplc.DefaultUni.Data, List())))
            case _ =>
                fail(s"Expected success, got: ${result1}")
        }

        val originSir2 = withDecls(
          SIR.Constr(
            "TxId",
            txIdData,
            List(SIR.Const(asConstant(hex"DEADBEEF"), ByteString, ae)),
            txIdData.constrType("TxId"),
            ae
          )
        )

        val uplc2 = lower(originSir2)
        println(s"lowered TxId constr: ${uplc2.pretty.render(100)}")

        // lowersTo (lam("hash", "TxId")(vr"TxId" $ vr"hash") $ hex"DEADBEEF")

    }

    test("lower And, Or, Not") {
        /* And True False
       lowers to (\True False -> And True False) True False
         */
        val a = SIR.Var("a", SIRType.Boolean, ae)
        val b = SIR.Var("b", SIRType.Boolean, ae)
        SIR.Let(
          NonRec,
          List(
            Binding("a", SIR.Const(Constant.Bool(true), SIRType.Boolean, ae)),
            Binding(
              "b",
              SIR.Const(Constant.Bool(true), SIRType.Boolean, ae)
            )
          ),
          SIR.And(a, b, ae),
          ae
        )
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
        val a1TypeVar = SIRType.TypeVar("A1", Some(1))
        val a2TypeVar = SIRType.TypeVar("A2", Some(2))
        val nilConstr = ConstrDecl("Nil", SIRVarStorage.DEFAULT, List(), List(), List(), ae)
        val consConstr = ConstrDecl(
          "Cons",
          SIRVarStorage.DEFAULT,
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
              SIRVarStorage.DEFAULT,
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
