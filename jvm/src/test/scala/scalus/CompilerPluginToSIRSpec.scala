package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.Compiler.compileDebug
import scalus.Compiler.fieldAsData
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.given
import scalus.ledger.api.v1.*
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.prelude.Prelude.given
import scalus.sir.{Binding, ConstrDecl, DataDecl, Recursivity, SIR, SIRBuiltins, SIRType, SIRUnify, SIRVarStorage, ToExprHSSIRFlat, TypeBinding}
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.SIRType.{Boolean, TypeVar}
import scalus.sir.SirDSL.{*, given}
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM

import scala.collection.immutable
import scala.language.implicitConversions
import scalus.uplc.eval.Result
import SIRType.Fun
import SIRType.TypeVar
import SIRType.TypeLambda

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
    private given PlutusVM = PlutusVM.makePlutusV2VM()
    val deadbeef = Constant.ByteString(hex"deadbeef")

    val sirData = SIRType.Data
    val sirBool = SIRType.Boolean
    val sirInt = SIRType.Integer
    val sirString = SIRType.String
    val sirByteString = SIRType.ByteString
    val sirVoid = SIRType.Unit
    def sirList(tpe: SIRType) = SIRType.List(tpe)
    def sirPair(t1: SIRType, t2: SIRType) = SIRType.Pair(t1, t2)

    def sirConst(x: Int) = Const(Constant.Integer(x), SIRType.Integer)
    def sirConst(x: BigInt) = Const(Constant.Integer(x), SIRType.Integer)
    def sirConst(x: Boolean) = Const(Constant.Bool(x), SIRType.Boolean)
    def sirConst(x: String) = Const(Constant.String(x), SIRType.String)
    def sirConst(x: ByteString) = Const(Constant.ByteString(x), SIRType.ByteString)
    def sirConst(x: Data) = Const(Constant.Data(x), SIRType.Data)
    def sirConstUnit = Const(Constant.Unit, SIRType.Unit)

    test("compile literals") {
        assert(compile(false) == Const(Constant.Bool(false), SIRType.Boolean))
        assert(compile(true) == Const(Constant.Bool(true), SIRType.Boolean))
        assert(compile(()) == Const(Constant.Unit, SIRType.Unit))
        assert(compile("foo") == Const(Constant.String("foo"), SIRType.String))
        assert(
          compile(BigInt("15511210043330985984000000")) == Const(
            Constant.Integer(BigInt("15511210043330985984000000")),
            SIRType.Integer
          )
        )
        assert(
          compile(12: BigInt) == Const(Constant.Integer(BigInt("12")), SIRType.Integer)
        )
        assert(
          compile(scala.math.BigInt.int2bigInt(12)) == Const(
            Constant.Integer(BigInt("12")),
            SIRType.Integer
          )
        )

        // ByteStrings
        assert(
          compile(builtin.ByteString.empty) == Const(
            Constant.ByteString(builtin.ByteString.empty),
            SIRType.ByteString
          )
        )

        assert(
          compile(builtin.ByteString.fromHex("deadbeef")) == Const(
            deadbeef,
            SIRType.ByteString
          )
        )
        assert(compile(hex"deadbeef") == Const(deadbeef, SIRType.ByteString))
        assert(
          compile(builtin.ByteString.fromString("deadbeef")) == Const(
            Constant.ByteString(builtin.ByteString.fromString("deadbeef")),
            SIRType.ByteString
          )
        )
    }

    test("compile if-then-else") {
        assert(
          compile {
              if Builtins.equalsInteger(1, 2) then () else ()
          } == SIR.IfThenElse(
            Apply(
              Apply(
                SIRBuiltins.equalsInteger,
                Const(Constant.Integer(1), SIRType.Integer),
                SIRType.Fun(SIRType.Integer, SIRType.Boolean)
              ),
              Const(Constant.Integer(2), SIRType.Integer),
              SIRType.Boolean
            ),
            Const(Constant.Unit, SIRType.Unit),
            Const(Constant.Unit, SIRType.Unit),
            SIRType.Unit
          )
        )
    }

    test("compile val def") {
        assert(
          compile {
              val a = true
              a
          } == Let(
            Recursivity.NonRec,
            immutable.List(Binding("a", Const(Constant.Bool(true), SIRType.Boolean))),
            Var("a", SIRType.Boolean)
          )
        )
    }

    test("compile def") {
        assert(
          compile {
              def b() = true

              def c(x: Boolean) = x

              c(b())
          } == Let(
            Recursivity.Rec,
            immutable.List(
              Binding(
                "b",
                LamAbs(
                  Var("_", SIRType.Unit),
                  Const(Constant.Bool(true), SIRType.Boolean)
                )
              )
            ),
            Let(
              Recursivity.Rec,
              immutable.List(
                Binding(
                  "c",
                  LamAbs(Var("x", SIRType.Boolean), Var("x", SIRType.Boolean))
                )
              ),
              Apply(
                Var("c", SIRType.Fun(Boolean, SIRType.Boolean)),
                Apply(
                  Var("b", SIRType.Fun(SIRType.Unit, SIRType.Boolean)),
                  Const(Constant.Unit, SIRType.Unit),
                  SIRType.Boolean
                ),
                SIRType.Boolean
              )
            )
          )
        )
    }

    test("compile lambda with args with type parameters") {
        // tail has a MethodType, check if it compiles
        val sir = compile {
            (tail: [A] => builtin.List[A] => builtin.List[A], ctx: builtin.List[Data]) =>
                tail[Data](ctx)
        }

        val compiledTp = sir.tp

        val a = TypeVar("A", Some(1))
        val listA = SIRType.List(TypeVar("A", Some(1)))
        val listData = SIRType.List(SIRType.Data)
        val tailType = SIRType.TypeLambda(List(a), Fun(listA, listA))

        val constructedExpr = LamAbs(
          Var("tail", tailType),
          LamAbs(Var("ctx", listData), Apply(Var("tail", tailType), Var("ctx", listData), listData))
        )

        // SIRUnify.unifyType(sir.asInstanceOf[SIRExpr].tp, constructedExpr.tp, SIRUnify.Env.empty.copy(debug = true)) match {
        //    case success@SIRUnify.UnificationSuccess(env,tp) => println("unifyType success")
        //    case failure@SIRUnify.UnificationFailure(path, left, right) => println(s"unifyType failure: ${failure}")
        // }

        assert(
          sir ~=~ LamAbs(
            Var("tail", tailType),
            LamAbs(
              Var("ctx", listData),
              Apply(Var("tail", tailType), Var("ctx", listData), listData)
            )
          )
        )

    }

    test("compile inline def") {
        assert(
          compile {
              inline def b = true

              b
          } == Const(Constant.Bool(true), sirBool)
        )
    }

    test("compile lambda") {
        assert(
          compile {
              val a = (x: Boolean) => x
              a(true)
          } ~=~ Let(
            NonRec,
            List(Binding("a", LamAbs(Var("x", sirBool), Var("x", sirBool)))),
            Apply(
              Var("a", SIRType.Fun(sirBool, sirBool)),
              Const(Constant.Bool(true), sirBool),
              sirBool
            )
          )
        )
    }

    test("compile throw") {
        assert(compile {
            throw new RuntimeException("foo")
        } ~=~ Error("foo"))
    }

    test("compile ToData") {
        import scalus.builtin.Data.*
        import scalus.builtin.ToDataInstances.given
        val compiled = compile {
            BigInt(1).toData
        }
        assert(
          compiled ~=~ Let(
            Rec,
            immutable.List(
              Binding(
                "scalus.builtin.ToDataInstances$.given_ToData_BigInt",
                LamAbs(Var("a", sirInt), Apply(SIRBuiltins.iData, Var("a", sirInt), sirData))
              )
            ),
            Let(
              NonRec,
              immutable.List(Binding("a$proxy1", Const(Constant.Integer(1), sirInt))),
              Apply(
                Var("scalus.builtin.ToDataInstances$.given_ToData_BigInt", Fun(sirInt, sirData)),
                Var("a$proxy1", sirInt),
                sirData
              )
            )
          )
        )
        //    val term = compiled.toUplc()
        //    assert(VM.evaluateTerm(term) == Data.I(22))
    }

    test("compile chooseList builtins") {

        assert(
          compile(
            Builtins.chooseList(builtin.List[BigInt](1, 2, 3), true, false)
          ) ~=~ (DefaultFun.ChooseList $ List(1, 2, 3) $ true $ false)
        )
    }

    test("compile mkCons builtins") {
        assert(
          compile(
            Builtins.mkCons(BigInt(4), builtin.List[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.MkCons $ 4 $ List(1, 2, 3))
        )
    }

    test("compile headList builtins") {
        assert(
          compile(
            Builtins.headList(builtin.List[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.HeadList $ List(1, 2, 3))
        )
    }

    test("compile tailList builtins") {
        assert(
          compile(
            Builtins.tailList(builtin.List[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.TailList $ List(1, 2, 3))
        )
    }

    test("compile nullList builtins") {
        assert(
          compile(
            Builtins.nullList(builtin.List[BigInt](1, 2, 3))
          ) ~=~ (DefaultFun.NullList $ List(1, 2, 3))
        )
    }

    test("compile empty List") {
        assert(
          compile {
              builtin.List.empty[BigInt]
          } ~=~ Const(
            Constant.List(DefaultUni.Integer, List()),
            SIRType.List(SIRType.Integer)
          )
        )
    }

    test("compile List literal") {
        assert(
          compile {
              builtin.List[BigInt](1, 2, 3)
          } ~=~ Const(
            Constant.List(
              DefaultUni.Integer,
              List(Constant.Integer(1), Constant.Integer(2), Constant.Integer(3))
            ),
            SIRType.List(SIRType.Integer)
          )
        )
    }

    test("compile MkCons builtin") {

        val compiled = compile {
            val a = "foo"
            "bar" :: builtin.List(a)
        }

        val expected = Let(
          NonRec,
          List(Binding("a", Const(Constant.String("foo"), sirString))),
          Apply(
            Apply(
              SIRBuiltins.mkCons,
              Const(Constant.String("bar"), sirString),
              Fun(SIRType.List(sirString), SIRType.List(sirString))
            ),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Var("a", sirString),
                Fun(SIRType.List(sirString), SIRType.List(sirString))
              ),
              Const(Constant.List(DefaultUni.String, List()), SIRType.List(sirString)),
              SIRType.List(sirString)
            ),
            SIRType.List(sirString)
          )
        )

        assert(
          compiled ~=~ expected
        )
    }

    test("compile head function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.head }
              ~=~ LamAbs(
                Var("l", sirList(sirInt)),
                Apply(SIRBuiltins.headList, Var("l", sirList(sirInt)), sirInt)
              )
        )
    }

    test("compile tail function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.tail }
              ~=~ LamAbs(
                Var("l", sirList(sirInt)),
                Apply(SIRBuiltins.tailList, Var("l", sirList(sirInt)), sirList(sirInt))
              )
        )
    }

    test("compile isEmpty function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.isEmpty }
              ~=~ LamAbs(
                Var("l", sirList(sirInt)),
                Apply(SIRBuiltins.nullList, Var("l", sirList(sirInt)), sirBool)
              )
        )
    }

    test("compile mkNilData") {
        assert(
          compile(Builtins.mkNilData())
              ~=~
                  (Apply(SIRBuiltins.mkNilData, Const(Constant.Unit, sirVoid), sirList(sirData)))
        )
    }

    test("compile mkNilPairData") {

        assert(
          compile(Builtins.mkNilPairData())
              ~=~
                  (Apply(
                    SIRBuiltins.mkNilPairData,
                    Const(Constant.Unit, sirVoid),
                    SIRType.List(SIRType.Pair(sirData, sirData))
                  ))
        )
    }

    test("compile mkConstr builtins") {
        val nilData = Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.List(sirData))
        assert(
          compile(
            Builtins.constrData(
              1,
              builtin.List(Builtins.iData(2))
            )
          ) ~=~ Apply(
            Apply(SIRBuiltins.constrData, sirConst(1), Fun(sirList(sirData), sirData)),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Apply(SIRBuiltins.iData, sirConst(2), sirData),
                Fun(sirList(sirData), sirList(sirData))
              ),
              nilData,
              sirList(sirData)
            ),
            sirData
          )
        )
    }

    test("compile mkList builtins") {
        val nilData = Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.List(sirData))
        assert(
          compile(
            Builtins.listData(builtin.List(Builtins.iData(1)))
          )
              ~=~
                  Apply(
                    SIRBuiltins.listData,
                    Apply(
                      Apply(
                        SIRBuiltins.mkCons,
                        Apply(SIRBuiltins.iData, sirConst(1), sirData),
                        Fun(sirList(sirData), sirList(sirData))
                      ),
                      nilData,
                      sirList(sirData)
                    ),
                    sirData
                  )
        )
    }

    test("compile mkPairData builtins") {

        val compiled = compile(builtin.Pair(Builtins.bData(hex"deadbeef"), Builtins.iData(1)))

        val expected =
            Apply(
              Apply(
                SIRBuiltins.mkPairData,
                Apply(SIRBuiltins.bData, sirConst(hex"deadbeef"), sirData),
                Fun(sirData, sirPair(sirData, sirData))
              ),
              Apply(SIRBuiltins.iData, sirConst(1), sirData),
              sirPair(sirData, sirData)
            )

        assert(
          compiled ~=~ expected
        )

    }

    test("compile mkMap builtins") {

        assert(
          compile(
            Builtins.mapData(
              builtin.List(
                builtin.Pair(Builtins.bData(hex"deadbeef"), Builtins.iData(1))
              )
            )
          ) ~=~ Apply(
            SIRBuiltins.mapData,
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Apply(
                  Apply(
                    SIRBuiltins.mkPairData,
                    Apply(SIRBuiltins.bData, Const(deadbeef, sirByteString), sirData),
                    Fun(sirData, sirPair(sirData, sirData))
                  ),
                  Apply(SIRBuiltins.iData, sirConst(1), sirData),
                  sirPair(sirData, sirData)
                ),
                Fun(sirList(sirPair(sirData, sirData)), sirList(sirPair(sirData, sirData)))
              ),
              Const(
                Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), immutable.Nil),
                SIRType.List(sirPair(sirData, sirData))
              ),
              sirList(sirPair(sirData, sirData))
            ),
            sirData
          )
        )
    }

    test("compile unsafeDataAsConstr function") {
        assert(
          compile { (d: Data) => Builtins.unConstrData(d) }
              ~=~ LamAbs(
                Var("d", sirData),
                Apply(
                  SIRBuiltins.unConstrData,
                  Var("d", sirData),
                  sirPair(sirInt, sirList(sirData))
                )
              )
        )
    }

    test("compile unsafeDataAsList function") {
        assert(
          compile { (d: Data) => Builtins.unListData(d) }
              ~=~ LamAbs(
                Var("d", sirData),
                Apply(SIRBuiltins.unListData, Var("d", sirData), sirList(sirData))
              )
        )
    }

    test("compile unsafeDataAsMap function") {
        assert(
          compile { (d: Data) => Builtins.unMapData(d) }
              ~=~ LamAbs(
                Var("d", sirData),
                Apply(SIRBuiltins.unMapData, Var("d", sirData), sirList(sirPair(sirData, sirData)))
              )
        )
    }

    test("compile unsafeDataAsB function") {
        assert(
          compile { (d: Data) => Builtins.unBData(d) }
              ~=~ LamAbs(
                Var("d", sirData),
                Apply(SIRBuiltins.unBData, Var("d", sirData), sirByteString)
              )
        )
    }

    test("compile unsafeDataAsI function") {
        assert(
          compile { (d: Data) => Builtins.unIData(d) } ~=~
              LamAbs(Var("d", sirData), Apply(SIRBuiltins.unIData, Var("d", sirData), sirInt))
        )
    }

    test("compile chooseData function") {
        assert(
          compile { (d: Data) => Builtins.chooseData[BigInt](d, 1, 2, 3, 4, 5) }
              ~=~ LamAbs(Var("d", sirData), ChooseData $ Var("d", sirData) $ 1 $ 2 $ 3 $ 4 $ 5)
        )
    }

    test("compile equalsData function") {
        assert(
          compile { (d1: Data, d2: Data) => Builtins.equalsData(d1, d2) }
              ~=~ LamAbs(
                Var("d1", sirData),
                LamAbs(
                  Var("d2", sirData),
                  SIRBuiltins.equalsData $ Var("d1", sirData) $ Var("d2", sirData)
                )
              )
        )
    }

    test("compile serialiseData builtins") {
        assert(
          compile {
              Builtins.serialiseData
          } ~=~ LamAbs(
            Var("d", sirData),
            Apply(SIRBuiltins.serialiseData, Var("d", sirData), sirByteString)
          )
        )
    }

    test("compile BLS12_381_G1 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G1_Element)
        val p2Var = Var("p2", SIRType.BLS12_381_G1_Element)
        val bsVar = Var("bs", SIRType.ByteString)
        val dstVar = Var("dst", SIRType.ByteString)

        assert(
          compile(Builtins.bls12_381_G1_add) ~=~ LamAbs(
            Var("p1", SIRType.BLS12_381_G1_Element),
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_add,
                  p1Var,
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element)
                ),
                p2Var,
                SIRType.BLS12_381_G1_Element
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_neg)
              ~=~ LamAbs(
                Var("p", SIRType.BLS12_381_G1_Element),
                Apply(
                  SIRBuiltins.bls12_381_G1_neg,
                  Var("p", SIRType.BLS12_381_G1_Element),
                  SIRType.BLS12_381_G1_Element
                )
              )
        )
        assert(
          compile(Builtins.bls12_381_G1_scalarMul) == LamAbs(
            Var("s", SIRType.Integer),
            LamAbs(
              Var("p", SIRType.BLS12_381_G1_Element),
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_scalarMul,
                  Var("s", SIRType.Integer),
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element)
                ),
                Var("p", SIRType.BLS12_381_G1_Element),
                SIRType.BLS12_381_G1_Element
              )
            )
          )
        )

        assert(
          compile(Builtins.bls12_381_G1_equal) == LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_equal,
                  p1Var,
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.Boolean)
                ),
                p2Var,
                SIRType.Boolean
              )
            )
          )
        )

        assert(
          compile(Builtins.bls12_381_G1_hashToGroup) == LamAbs(
            bsVar,
            LamAbs(
              dstVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_hashToGroup,
                  bsVar,
                  Fun(SIRType.ByteString, SIRType.BLS12_381_G1_Element)
                ),
                dstVar,
                SIRType.BLS12_381_G1_Element
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_compress) ~=~ LamAbs(
            Var("p", SIRType.BLS12_381_G1_Element),
            Apply(
              SIRBuiltins.bls12_381_G1_compress,
              Var("p", SIRType.BLS12_381_G1_Element),
              SIRType.ByteString
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_uncompress) ~=~ LamAbs(
            Var("bs", SIRType.ByteString),
            Apply(
              SIRBuiltins.bls12_381_G1_uncompress,
              Var("bs", SIRType.ByteString),
              SIRType.BLS12_381_G1_Element
            )
          )
        )
    }

    test("compile BLS12_381_G2 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G2_Element)
        val p2Var = Var("p2", SIRType.BLS12_381_G2_Element)
        val pVar = Var("p", SIRType.BLS12_381_G2_Element)
        val sBlsVar = Var("s", SIRType.BLS12_381_G2_Element)
        val sIntVar = Var("s", SIRType.Integer)
        val bsVar = Var("bs", SIRType.ByteString)
        val dstVar = Var("dst", SIRType.ByteString)
        assert(
          compile(Builtins.bls12_381_G2_add) ~=~ LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_add,
                  p1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element)
                ),
                p2Var,
                SIRType.BLS12_381_G2_Element
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_neg) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_neg, pVar, SIRType.BLS12_381_G2_Element)
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_scalarMul) ~=~ LamAbs(
            sIntVar,
            LamAbs(
              pVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_scalarMul,
                  sIntVar,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element)
                ),
                pVar,
                SIRType.BLS12_381_G2_Element
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_equal) ~=~ LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_equal,
                  p1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.Boolean)
                ),
                p2Var,
                SIRType.Boolean
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_hashToGroup) ~=~ LamAbs(
            bsVar,
            LamAbs(
              dstVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_hashToGroup,
                  bsVar,
                  Fun(SIRType.ByteString, SIRType.BLS12_381_G2_Element)
                ),
                dstVar,
                SIRType.BLS12_381_G2_Element
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_compress) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_compress, pVar, SIRType.ByteString)
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_uncompress) == LamAbs(
            bsVar,
            Apply(SIRBuiltins.bls12_381_G2_uncompress, bsVar, SIRType.BLS12_381_G2_Element)
          )
        )
    }

    test("compile BLS12_381 pairing operations builtins") {
        val p1BlsG1Var = Var("p1", SIRType.BLS12_381_G1_Element)
        val p2BlsG2Var = Var("p2", SIRType.BLS12_381_G2_Element)
        val p1BlsMlVar = Var("p1", SIRType.BLS12_381_MlResult)
        val p2BlsMlVar = Var("p2", SIRType.BLS12_381_MlResult)
        val r1Var = Var("r1", SIRType.BLS12_381_MlResult)
        val r2Var = Var("r2", SIRType.BLS12_381_MlResult)

        assert(
          compile(Builtins.bls12_381_millerLoop) ~=~ LamAbs(
            p1BlsG1Var,
            LamAbs(
              p2BlsG2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_millerLoop,
                  p1BlsG1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_MlResult)
                ),
                p2BlsG2Var,
                SIRType.BLS12_381_MlResult
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_mulMlResult) ~=~ LamAbs(
            r1Var,
            LamAbs(
              r2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_mulMlResult,
                  r1Var,
                  Fun(SIRType.BLS12_381_MlResult, SIRType.BLS12_381_MlResult)
                ),
                r2Var,
                SIRType.BLS12_381_MlResult
              )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_finalVerify) ~=~ LamAbs(
            p1BlsMlVar,
            LamAbs(
              p2BlsMlVar,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_finalVerify,
                  p1BlsMlVar,
                  Fun(SIRType.BLS12_381_MlResult, SIRType.Boolean)
                ),
                p2BlsMlVar,
                SIRType.Boolean
              )
            )
          )
        )
    }

    test("compile Keccak_256 builtin") {
        val bsVar = Var("bs", SIRType.ByteString)
        assert(
          compile(Builtins.keccak_256) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.keccak_256, bsVar, SIRType.ByteString)
          )
        )
    }

    test("compile Blake2b_224 builtin") {
        val bsVar = Var("bs", SIRType.ByteString)
        assert(
          compile(Builtins.blake2b_224) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.blake2b_224, bsVar, SIRType.ByteString)
          )
        )
    }

    test("compile BigInt ops") {
        assert(compile(-BigInt(-1)) == (SubtractInteger $ 0 $ -1))
        assert(compile(BigInt(1) + 2) == (AddInteger $ 1 $ 2))
        assert(compile(BigInt(1) - 2) == (SubtractInteger $ 1 $ 2))
        assert(compile(BigInt(1) * 2) == (MultiplyInteger $ 1 $ 2))
        assert(compile(BigInt(1) / 2) == (DivideInteger $ 1 $ 2))
        assert(compile(BigInt(1) % 2) == (RemainderInteger $ 1 $ 2))
        assert(compile(BigInt(1) < 2) == (LessThanInteger $ 1 $ 2))
        assert(compile(BigInt(1) <= 2) == (LessThanEqualsInteger $ 1 $ 2))
        assert(compile(BigInt(1) > 2) == (LessThanInteger $ 2 $ 1))
        assert(compile(BigInt(1) >= 2) == (LessThanEqualsInteger $ 2 $ 1))
        assert(compile(BigInt(1) == BigInt(2)) == (EqualsInteger $ 1 $ 2))
        assert(compile(BigInt(1) != BigInt(2)) == Not(EqualsInteger $ 1 $ 2))
    }

    test("compile Integer builtins") {
        assert(compile(Builtins.addInteger(1, 2)) == (AddInteger $ 1 $ 2))
        assert(compile(Builtins.subtractInteger(1, 2)) == (SubtractInteger $ 1 $ 2))
        assert(compile(Builtins.multiplyInteger(1, 2)) == (MultiplyInteger $ 1 $ 2))
        assert(compile(Builtins.divideInteger(1, 2)) == (DivideInteger $ 1 $ 2))
        assert(compile(Builtins.modInteger(1, 2)) == (ModInteger $ 1 $ 2))
        assert(compile(Builtins.quotientInteger(1, 2)) == (QuotientInteger $ 1 $ 2))
        assert(compile(Builtins.remainderInteger(1, 2)) == (RemainderInteger $ 1 $ 2))
        assert(compile(Builtins.lessThanInteger(1, 2)) == (LessThanInteger $ 1 $ 2))
        assert(compile(Builtins.lessThanEqualsInteger(1, 2)) == (LessThanEqualsInteger $ 1 $ 2))
        assert(compile(Builtins.equalsInteger(1, 2)) == (EqualsInteger $ 1 $ 2))
    }

    test("compile ByteStrings builtins") {
        assert(
          compile(
            Builtins.appendByteString(hex"dead", hex"beef")
          ) == (AppendByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.sliceByteString(1, 2, hex"dead")
          ) == (SliceByteString $ 1 $ 2 $ hex"dead")
        )

        assert(
          compile(
            Builtins.lengthOfByteString(hex"dead")
          ) == (LengthOfByteString $ hex"dead")
        )

        assert(
          compile(
            Builtins.indexByteString(hex"dead", 1)
          ) == (IndexByteString $ hex"dead" $ 1)
        )

        assert(
          compile(
            Builtins.equalsByteString(hex"dead", hex"beef")
          ) == (EqualsByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.lessThanByteString(hex"dead", hex"beef")
          ) == (LessThanByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.lessThanEqualsByteString(hex"dead", hex"beef")
          ) == (LessThanEqualsByteString $ hex"dead" $ hex"beef")
        )
    }

    test("compile Crypto builtins") {
        /*
            // Cryptography and hashes
          case Sha2_256
          case Sha3_256
          case Blake2b_256
          case VerifyEd25519Signature // formerly verifySignature
          case VerifyEcdsaSecp256k1Signature
          case VerifySchnorrSecp256k1Signature
         */
        assert(compile(Builtins.sha2_256(hex"dead")) == (Sha2_256 $ hex"dead"))
        assert(compile(Builtins.sha3_256(hex"dead")) == (Sha3_256 $ hex"dead"))
        assert(compile(Builtins.blake2b_256(hex"dead")) == (Blake2b_256 $ hex"dead"))
        assert(
          compile(
            Builtins.verifyEd25519Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) == (VerifyEd25519Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
        assert(
          compile(
            Builtins.verifyEcdsaSecp256k1Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) == (VerifyEcdsaSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
        assert(
          compile(
            Builtins.verifySchnorrSecp256k1Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) == (VerifySchnorrSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
    }

    test("compile String builtins") {
        assert(compile(Builtins.appendString("dead", "beef")) == (AppendString $ "dead" $ "beef"))
        assert(compile(Builtins.equalsString("dead", "beef")) == (EqualsString $ "dead" $ "beef"))
        assert(compile(Builtins.encodeUtf8("dead")) == (EncodeUtf8 $ "dead"))
        assert(compile(Builtins.decodeUtf8(hex"dead")) == (DecodeUtf8 $ hex"dead"))
    }

    test("compile IfThenElse/ChooseUnit/Trace builtins") {
        assert(
          compile(
            Builtins.ifThenElse(true, BigInt(1), BigInt(2))
          ) ~=~ (DefaultFun.IfThenElse $ true $ 1 $ 2)
        )
        // TODO: check if that is correct
        assert(compile(Builtins.chooseUnit()(true)) ~=~ (DefaultFun.ChooseUnit $ () $ true))
        assert(compile(Builtins.trace("dead")(BigInt(1))) ~=~ (DefaultFun.Trace $ "dead" $ 1))
    }

    test("compile Pair builtins") {
        val fst = Var("fst", sirData)
        val snd = Var("snd", sirData)
        val pv = Var("p", SIRType.Pair(sirData, sirData))

        assert(
          compile(Builtins.mkPairData) ~=~ LamAbs(
            fst,
            LamAbs(
              snd,
              Apply(
                Apply(SIRBuiltins.mkPairData, fst, Fun(sirData, SIRType.Pair(sirData, sirData))),
                snd,
                SIRType.Pair(sirData, sirData)
              )
            )
          )
        )

        assert(
          compile { (p: builtin.Pair[Data, Data]) =>
              builtin.Pair(Builtins.sndPair(p), Builtins.fstPair(p))
          } ~=~
              LamAbs(
                pv,
                (SIRBuiltins.mkPairData $ (SIRBuiltins.sndPair $ pv)) $ (SIRBuiltins.fstPair $ pv)
              )
        )

        assert(
          compile { builtin.Pair(BigInt(1), hex"deadbeef") }
              ~=~ Const(
                Constant.Pair(Constant.Integer(1), deadbeef),
                SIRType.Pair(sirInt, sirByteString)
              )
        )

        assert(
          compile { (p: builtin.Pair[Data, Data]) => builtin.Pair(p.snd, p.fst) }
              ~=~ LamAbs(
                pv,
                Apply(
                  Apply(
                    SIRBuiltins.mkPairData,
                    Apply(SIRBuiltins.sndPair, pv, sirData),
                    Fun(sirData, SIRType.Pair(sirData, sirData))
                  ),
                  Apply(SIRBuiltins.fstPair, pv, sirData),
                  SIRType.Pair(sirData, sirData)
                )
              )
        )

    }

    test("compile Boolean &&, ||, ! builtins") {
        import Constant.Bool
        val compiled = compile {
            val a = true || (throw new Exception("M"))
            !a && false || true
        }
        assert(
          compiled ~=~
              Let(
                NonRec,
                List(Binding("a", Or(sirConst(true), Error("M")))),
                Or(And(Not(Var("a", sirBool)), sirConst(false)), sirConst(true))
              )
        )
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
    }

    test("compile Boolean equality") {
        import Constant.Bool
        val eq = compile { def check(a: Boolean) = a == false; check }
        val ne = compile { def check(a: Boolean) = a != false; check }
        val aVar = Var("a", sirBool)

        assert(
          eq ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  aVar,
                  SIR.IfThenElse(
                    aVar,
                    sirConst(false),
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool),
                    sirBool
                  )
                )
              )
            ),
            LamAbs(aVar, Apply(Var("check", Fun(sirBool, sirBool)), aVar, sirBool))
          )
        )

        assert(
          ne ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  aVar,
                  SIR.IfThenElse(
                    aVar,
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool),
                    sirConst(false),
                    sirBool
                  )
                )
              )
            ),
            LamAbs(aVar, Apply(Var("check", Fun(sirBool, sirBool)), aVar, sirBool))
          )
        )

        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        assert((eqterm $ true).evaluate == scalus.uplc.Term.Const(asConstant(false)))
        assert((eqterm $ false).evaluate == scalus.uplc.Term.Const(asConstant(true)))
        assert((neterm $ true).evaluate == scalus.uplc.Term.Const(asConstant(true)))
        assert((neterm $ false).evaluate == scalus.uplc.Term.Const(asConstant(false)))
    }

    test("compile ByteString equality") {
        val eqCompiled = compile { def check(a: ByteString, b: ByteString) = a == b; check }

        val eqExpected = Let(
          Rec,
          List(
            Binding(
              "check",
              LamAbs(
                Var("a", sirByteString),
                LamAbs(
                  Var("b", sirByteString),
                  Apply(
                    Apply(
                      SIRBuiltins.equalsByteString,
                      Var("a", sirByteString),
                      Fun(sirByteString, sirBool)
                    ),
                    Var("b", sirByteString),
                    sirBool
                  )
                )
              )
            )
          ),
          LamAbs(
            Var("a", sirByteString),
            LamAbs(
              Var("b", sirByteString),
              Apply(
                Apply(
                  Var("check", Fun(sirByteString, Fun(sirByteString, sirBool))),
                  Var("a", sirByteString),
                  Fun(sirByteString, sirBool)
                ),
                Var("b", sirByteString),
                sirBool
              )
            )
          )
        )

        assert(
          eqCompiled ~=~ eqExpected
        )

        val ne = compile {
            def check(a: ByteString, b: ByteString) = a != b; check
        }

        assert(
          ne ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  Var("a", sirByteString),
                  LamAbs(
                    Var("b", sirByteString),
                    Not(
                      Apply(
                        Apply(
                          SIRBuiltins.equalsByteString,
                          Var("a", sirByteString),
                          Fun(sirByteString, sirBool)
                        ),
                        Var("b", sirByteString),
                        sirBool
                      )
                    )
                  )
                )
              )
            ),
            LamAbs(
              Var("a", sirByteString),
              LamAbs(
                Var("b", sirByteString),
                Apply(
                  Apply(
                    Var("check", Fun(sirByteString, Fun(sirByteString, sirBool))),
                    Var("a", sirByteString),
                    Fun(sirByteString, sirBool)
                  ),
                  Var("b", sirByteString),
                  sirBool
                )
              )
            )
          )
        )

        val eqterm = eqCompiled.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        assert(
          (eqterm $ ByteString.empty $ ByteString.empty).evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          (eqterm $ ByteString.empty $ hex"deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ ByteString.empty $ ByteString.empty).evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ ByteString.empty $ hex"deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )

    }

    test("compile String equality") {
        val eq = compile { def check(a: String, b: String) = a == b; check }

        val aVar = Var("a", sirString)
        val bVar = Var("b", sirString)

        assert(
          eq ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  aVar,
                  LamAbs(
                    bVar,
                    Apply(
                      Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool)),
                      bVar,
                      sirBool
                    )
                  )
                )
              )
            ),
            LamAbs(
              aVar,
              LamAbs(
                bVar,
                Apply(
                  Apply(
                    Var("check", Fun(sirString, Fun(sirString, sirBool))),
                    aVar,
                    Fun(sirString, sirBool)
                  ),
                  bVar,
                  sirBool
                )
              )
            )
          )
        )

        val ne = compile {
            def check(a: String, b: String) = a != b; check
        }

        val neExpected = Let(
          Rec,
          List(
            Binding(
              "check",
              LamAbs(
                aVar,
                LamAbs(
                  bVar,
                  Not(
                    Apply(
                      Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool)),
                      bVar,
                      sirBool
                    )
                  )
                )
              )
            )
          ),
          LamAbs(
            aVar,
            LamAbs(
              bVar,
              Apply(
                Apply(
                  Var("check", Fun(sirString, Fun(sirString, sirBool))),
                  aVar,
                  Fun(sirString, sirBool)
                ),
                bVar,
                sirBool
              )
            )
          )
        )

        assert(
          ne ~=~ neExpected
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        assert(
          (eqterm $ "" $ "").evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          (eqterm $ "" $ "deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ "" $ "").evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ "" $ "deadbeef").evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
    }

    test("compile Data equality") {
        val eq = compile { def check(a: Data, b: Data) = a == b; check }
        val ne = compile { def check(a: Data, b: Data) = a != b; check }
        val a = Var("a", sirData)
        val b = Var("b", sirData)
        val check = Var("check", Fun(sirData, Fun(sirData, sirBool)))
        assert(
          eq == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  a,
                  LamAbs(
                    b,
                    Apply(Apply(SIRBuiltins.equalsData, a, Fun(sirData, sirBool)), b, sirBool)
                  )
                )
              )
            ),
            LamAbs(a, LamAbs(b, Apply(Apply(check, a, Fun(sirData, sirBool)), b, sirBool)))
          )
        )
        assert(
          ne ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  a,
                  LamAbs(
                    b,
                    Not(Apply(Apply(SIRBuiltins.equalsData, a, Fun(sirData, sirBool)), b, sirBool))
                  )
                )
              )
            ),
            LamAbs(a, LamAbs(b, Apply(Apply(check, a, Fun(sirData, sirBool)), b, sirBool)))
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        import scalus.builtin.Data.toData
        import scalus.builtin.ToDataInstances.given

        assert(
          (eqterm $ 1.toData $ 1.toData).evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          (eqterm $ "".toData $ "deadbeef".toData).evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ 1.toData $ 1.toData).evaluate == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          (neterm $ "".toData $ "deadbeef".toData).evaluate == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
    }

    test("compile type-safe equality") {
        import scalus.prelude.Prelude.*
        val compiled = compile {
            val a = BigInt(0)
            val bs = hex"deadbeef"
            val s = "string"
            a === a && bs === bs && s === s
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
    }

    test("compile external definitions") {
        def foo(i: BigInt) = i

        val iVar = Var("i", sirInt)

        assert(
          compile {
              foo(5)
          } ==
              Let(
                Rec,
                List(
                  Binding("scalus.CompilerPluginToSIRSpec._$_$foo", LamAbs(iVar, iVar))
                ),
                Apply(
                  Var("scalus.CompilerPluginToSIRSpec._$_$foo", Fun(sirInt, sirInt)),
                  sirConst(5),
                  sirInt
                )
              )
        )
    }

    private val pubKeyHashDataDecl = DataDecl(
      "scalus.ledger.api.v1.PubKeyHash",
      List(
        ConstrDecl(
          "PubKeyHash",
          SIRVarStorage.DEFAULT,
          List(TypeBinding("hash", sirByteString)),
          List.empty
        )
      ),
      List.empty
    )

    test("compile datatypes") {
        import scalus.ledger.api.v1.PubKeyHash
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh.hash
        }

        assert(
          compiled ~=~
              Decl(
                pubKeyHashDataDecl,
                Let(
                  NonRec,
                  List(
                    Binding(
                      "pkh",
                      Constr(
                        "PubKeyHash",
                        pubKeyHashDataDecl,
                        List(Const(uplc.Constant.ByteString(hex"DEADBEEF"), sirByteString))
                      )
                    )
                  ),
                  Select(Var("pkh", pubKeyHashDataDecl.tp), "hash", sirByteString)
                )
              )
        )
    }

    test("compile companion object apply as a primary constructor") {
        val compiled = compile {
            scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
        }

        assert(
          compiled ==
              Decl(
                pubKeyHashDataDecl,
                Constr(
                  "PubKeyHash",
                  pubKeyHashDataDecl,
                  List(Const(Constant.ByteString(hex"deadbeef"), SIRType.ByteString))
                )
              )
        )
    }

    test("compile Tuple2 construction/matching") {
        val compiled = compile {
            type Pair = (Boolean, Boolean)
            val t: Pair = (true, false)
            t match
                case (a, _) => a && t._2
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(false)))
    }

    test("compile match on a case class") {
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh match
                case PubKeyHash(hash) => hash
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        assert(evaled == scalus.uplc.Term.Const(Constant.ByteString(hex"deadbeef")))
    }

    test("compile match on ADT") {
        import scalus.prelude.List
        import scalus.prelude.List.*
        val compiled = compile {
            val ls: List[BigInt] = single(BigInt(1))
            ls match
                case Cons(h, _) => h
                case Nil        => BigInt(0)
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
    }

    test("compile wildcard match on ADT") {
        import scalus.prelude.These
        val compiled = compile {
            val t: These[BigInt, Boolean] = new These.This(BigInt(1))
            t match
                case These.This(h) => h
                case _             => BigInt(0)
        }
        val evaled = compiled.toUplc().evaluate
        assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
    }

    test("compile inner matches") {
        import scalus.prelude.List
        import scalus.prelude.List.*
        val compiled = compile {
            val ls: List[(BigInt, TxOutRef)] =
                cons((1, new TxOutRef(new TxId(hex"deadbeef"), 2)), Nil)
            ls match
                case Cons(h @ (a, TxOutRef(TxId(_), idx)), _) => a + idx
                case Nil                                      => BigInt(0)
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == scalus.uplc.Term.Const(Constant.Integer(3)))
    }

    test("compile multiple inner matches") {
        import scalus.prelude.List.*
        val compiled = compile {
            ((true, "test"), (false, "test")) match
                case ((a, _), (b, _)) => a == b
        }
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(false)))
    }

    test("compile fieldAsData macro") {
        import scalus.ledger.api.v1.*
        import scalus.ledger.api.v1.FromDataInstances.given
        import scalus.ledger.api.v1.ToDataInstances.given

        val compiled = compile { (ctx: scalus.builtin.Data) =>
            // check multiple nested fields
            val sigsData = fieldAsData[ScriptContext](_.txInfo.signatories)(ctx)
            // check type aliased fields
            val from = ctx.field[ScriptContext](_.txInfo.validRange.from).to[IntervalBound]
            // check tuples
            val data = ctx.field[(ByteString, Data)](_._2)
            val sigs = Builtins.unListData(sigsData)
            Builtins.unBData(sigs.head)
        }
        // println(compiled.show)
        val term = compiled.toUplc()

        val scriptContext =
            ScriptContext(
              TxInfo(
                Nil,
                Nil,
                Value.zero,
                Value.zero,
                Nil,
                Nil,
                Interval.always,
                Cons(PubKeyHash(hex"deadbeef"), Nil),
                Nil,
                TxId(hex"bb")
              ),
              ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
            )
        import scalus.uplc.TermDSL.given
        import scalus.builtin.Data.*
        import DefaultUni.asConstant
        val appliedScript = term.plutusV1 $ scriptContext.toData
        assert(appliedScript.evaluate == scalus.uplc.Term.Const(asConstant(hex"deadbeef")))
        val flatBytesLength = appliedScript.flatEncoded.length
        assert(flatBytesLength == 332)
    }

    test("@Ignore annotation") {
        @Ignore
        def foo() = 1

        assert(compile {
            @Ignore val a = true

            @Ignore def foo() = true
        } == Const(Constant.Unit, SIRType.Unit))
    }

    test("Ignore PlatformSpecific arguments") {
        // Make sure that the implicit PlatformSpecific argument is not generated
        assert(compile(Builtins.sha2_256) == (lam("bs")(Sha2_256 $ Var("bs", sirByteString))))
    }

    test("? operator produces a debug log") {
        import scalus.prelude.?
        val compiled = compile {
            val oneEqualsTwo = BigInt(1) == BigInt(2)
            oneEqualsTwo.?
        }
        val script = compiled.toUplc().plutusV2
        script.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Bool(false)))
                assert(logs == List("oneEqualsTwo ? False: { mem: 0.002334, cpu: 0.539980 }"))
            case Result.Failure(exception, _, _, _) => fail(exception)
    }

    test("compile large script") {
        // this test ensures that the compiler can handle large scripts
        inline def generate(n: Int): String =
            if n == 0 then "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
            else
                Builtins.appendString(
                  generate(n - 1),
                  "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                      + "asdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdf"
                )
        // this generates a script with 99 calls to appendString
        // appendString(appendString(..., "asdf..."), ..., "asdf...")
        val compiled = compile {
            generate(99)
        }
        assert(compiled.toUplc().plutusV3.flatEncoded.length == 93652)
    }
