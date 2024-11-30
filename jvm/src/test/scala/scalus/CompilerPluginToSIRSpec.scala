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
import scalus.sir.{Binding, ConstrDecl, DataDecl, Recursivity, SIR, SIRBuiltins, SIRExpr, SIRType, SIRUnify, SIRVarStorage, ToExprHSSIRFlat, TypeBinding}
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.SIRType.{BooleanPrimitive, TypeVar}
import scalus.sir.SirDSL.{*, given}
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.*
import scalus.uplc.eval.VM

import scala.collection.immutable
import scala.language.implicitConversions
import scalus.uplc.eval.Result
import SIRType.Fun
import SIRType.TypeVar
import SIRType.TypeLambda

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
    val deadbeef = Constant.ByteString(hex"deadbeef")

    val sirData = SIRType.Data
    val sirBool = SIRType.BooleanPrimitive
    val sirInt = SIRType.IntegerPrimitive
    val sirString = SIRType.StringPrimitive
    val sirByteString = SIRType.ByteStringPrimitive
    val sirVoid = SIRType.VoidPrimitive
    def sirList(tpe: SIRType) = SIRType.List(tpe)
    def sirPair(t1: SIRType, t2: SIRType) = SIRType.Pair(t1, t2)

    def sirConst(x:Int) = Const(Constant.Integer(x), SIRType.IntegerPrimitive)
    def sirConst(x:BigInt) = Const(Constant.Integer(x), SIRType.IntegerPrimitive)
    def sirConst(x:Boolean) = Const(Constant.Bool(x), SIRType.BooleanPrimitive)
    def sirConst(x:String) = Const(Constant.String(x), SIRType.StringPrimitive)
    def sirConst(x:ByteString) = Const(Constant.ByteString(x), SIRType.ByteStringPrimitive)
    def sirConst(x:Data) = Const(Constant.Data(x), SIRType.Data)
    def sirConstUnit = Const(Constant.Unit, SIRType.VoidPrimitive)



    test("compile literals") {
        assert(compile(false) == Const(Constant.Bool(false), SIRType.BooleanPrimitive))
        assert(compile(true) == Const(Constant.Bool(true), SIRType.BooleanPrimitive))
        assert(compile(()) == Const(Constant.Unit, SIRType.VoidPrimitive))
        assert(compile("foo") == Const(Constant.String("foo"), SIRType.StringPrimitive))
        assert(
          compile(BigInt("15511210043330985984000000")) == Const(
            Constant.Integer(BigInt("15511210043330985984000000")),
            SIRType.IntegerPrimitive
          )
        )
        assert(
          compile(12: BigInt) == Const(Constant.Integer(BigInt("12")), SIRType.IntegerPrimitive)
        )
        assert(
          compile(scala.math.BigInt.int2bigInt(12)) == Const(
            Constant.Integer(BigInt("12")),
            SIRType.IntegerPrimitive
          )
        )

        // ByteStrings
        assert(
          compile(builtin.ByteString.empty) == Const(
            Constant.ByteString(builtin.ByteString.empty),
            SIRType.ByteStringPrimitive
          )
        )

        assert(
          compile(builtin.ByteString.fromHex("deadbeef")) == Const(
            deadbeef,
            SIRType.ByteStringPrimitive
          )
        )
        assert(compile(hex"deadbeef") == Const(deadbeef, SIRType.ByteStringPrimitive))
        assert(
          compile(builtin.ByteString.fromString("deadbeef")) == Const(
            Constant.ByteString(builtin.ByteString.fromString("deadbeef")),
            SIRType.ByteStringPrimitive
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
                Const(Constant.Integer(1), SIRType.IntegerPrimitive),
                SIRType.Fun(SIRType.IntegerPrimitive, SIRType.BooleanPrimitive)
              ),
              Const(Constant.Integer(2), SIRType.IntegerPrimitive),
              SIRType.BooleanPrimitive
            ),
            Const(Constant.Unit, SIRType.VoidPrimitive),
            Const(Constant.Unit, SIRType.VoidPrimitive),
            SIRType.VoidPrimitive
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
            immutable.List(Binding("a", Const(Constant.Bool(true), SIRType.BooleanPrimitive))),
            Var("a", SIRType.BooleanPrimitive)
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
                  Var("_", SIRType.VoidPrimitive),
                  Const(Constant.Bool(true), SIRType.BooleanPrimitive)
                )
              )
            ),
            Let(
              Recursivity.Rec,
              immutable.List(
                Binding(
                  "c",
                  LamAbs(Var("x", SIRType.BooleanPrimitive), Var("x", SIRType.BooleanPrimitive))
                )
              ),
              Apply(
                Var("c", SIRType.Fun(BooleanPrimitive, SIRType.BooleanPrimitive)),
                Apply(
                  Var("b", SIRType.Fun(SIRType.VoidPrimitive, SIRType.BooleanPrimitive)),
                  Const(Constant.Unit, SIRType.VoidPrimitive),
                  SIRType.BooleanPrimitive
                ),
                SIRType.BooleanPrimitive
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

        val compiledTp = sir.asInstanceOf[SIRExpr].tp

        val a = TypeVar("A", Some(1))
        val listA = SIRType.List(TypeVar("A", Some(1)))
        val listData = SIRType.List(SIRType.Data)
        val tailType = SIRType.TypeLambda(List(a), Fun(listA, listA))

        val constructedExpr = LamAbs(
          Var("tail", tailType),
          LamAbs(Var("ctx", listData),
              Apply(Var("tail",tailType),
                    Var("ctx", listData),
                    listData
              )
          )
        )


        //SIRUnify.unifyType(sir.asInstanceOf[SIRExpr].tp, constructedExpr.tp, SIRUnify.Env.empty.copy(debug = true)) match {
        //    case success@SIRUnify.UnificationSuccess(env,tp) => println("unifyType success")
        //    case failure@SIRUnify.UnificationFailure(path, left, right) => println(s"unifyType failure: ${failure}")
        //}

        assert(
          sir ~=~ LamAbs(
            Var("tail", tailType),
            LamAbs(Var("ctx", listData),
                Apply(Var("tail",tailType),
                      Var("ctx", listData),
                      listData
                )
            )
          )
        )


    }


    test("compile inline def") {
        assert(
          compile {
              inline def b = true

              b
          } == Const(Constant.Bool(true),sirBool)
        )
    }

    test("compile lambda") {
        assert(
          compile {
              val a = (x: Boolean) => x
              a(true)
          } ~=~ Let(
            NonRec,
            List(Binding("a", LamAbs(Var("x",sirBool), Var("x", sirBool)))),
            Apply(Var("a", SIRType.Fun(sirBool,sirBool)),
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
                LamAbs(Var("a",sirInt), Apply(SIRBuiltins.iData , Var("a",sirInt), sirData))
              )
            ),
            Let(
              NonRec,
              immutable.List(Binding("a$proxy1", Const(Constant.Integer(1), sirInt))),
              Apply(Var("scalus.builtin.ToDataInstances$.given_ToData_BigInt", Fun(sirInt,sirData)), Var("a$proxy1",sirInt), sirData)
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
          } ~=~ Const(Constant.List(DefaultUni.Integer, List()), SIRType.List(SIRType.IntegerPrimitive))
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
            SIRType.List(SIRType.IntegerPrimitive)
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
              Apply(SIRBuiltins.mkCons, Const(Constant.String("bar"), sirString), Fun(SIRType.List(sirString), SIRType.List(sirString))),
              Apply(
                Apply(SIRBuiltins.mkCons, Var("a", sirString), Fun(SIRType.List(sirString), SIRType.List(sirString))),
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
              ~=~ LamAbs(Var("l",sirList(sirInt)), Apply(SIRBuiltins.headList, Var("l",sirList(sirInt)), sirInt))
        )
    }

    test("compile tail function") {
        assert(compile { (l: builtin.List[BigInt]) => l.tail }
            ~=~ LamAbs(Var("l",sirList(sirInt)), Apply(SIRBuiltins.tailList, Var("l",sirList(sirInt)), sirList(sirInt)))
        )
    }

    test("compile isEmpty function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.isEmpty }
              ~=~ LamAbs(Var("l",sirList(sirInt)), Apply(SIRBuiltins.nullList, Var("l",sirList(sirInt)), sirBool))
        )
    }

    test("compile mkNilData") {
        assert(compile(Builtins.mkNilData())
            ~=~
            (Apply(SIRBuiltins.mkNilData, Const(Constant.Unit, sirVoid), sirList(sirData)))
        )
    }

    test("compile mkNilPairData") {
        assert(
          compile(Builtins.mkNilPairData())
              ~=~
              (Apply(SIRBuiltins.mkNilPairData, Const(Constant.Unit, sirVoid), SIRType.Pair(sirData, sirData)))
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
              Apply(SIRBuiltins.mkCons, Apply(SIRBuiltins.iData, sirConst(2), sirData), Fun(sirList(sirData), sirList(sirData))),
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
              Apply(SIRBuiltins.mkCons, Apply(SIRBuiltins.iData, sirConst(1), sirData), Fun(sirList(sirData), sirList(sirData))),
              nilData,
              sirList(sirData)
            ),
            sirData
          )
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
                  Apply(SIRBuiltins.mkPairData,
                      Apply(SIRBuiltins.bData, Const(deadbeef, sirByteString), sirData),
                      Fun(sirData, sirPair(sirData,sirData)) ),
                  Apply(SIRBuiltins.iData, sirConst(1), sirInt),
                  sirPair(sirData,sirData)
                ),
                Fun(sirList(sirPair(sirData,sirData)), sirList(sirPair(sirData,sirData)))
              ),
              Const(Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), immutable.Nil),
                    SIRType.List(sirPair(sirData,sirData))
              ),
              sirList(sirPair(sirData,sirData))
            ),
            sirData
          )
        )
    }

    test("compile unsafeDataAsConstr function") {
        assert(
          compile { (d: Data) => Builtins.unConstrData(d) }
              ~=~ LamAbs(Var("d",sirData), Apply(SIRBuiltins.unConstrData, Var("d",sirData), sirPair(sirInt,sirList(sirData))))
        )
    }

    test("compile unsafeDataAsList function") {
        assert(
          compile { (d: Data) => Builtins.unListData(d) }
              ~=~ LamAbs(Var("d",sirData), Apply(SIRBuiltins.unListData, Var("d",sirData), sirList(sirData)))
        )
    }

    test("compile unsafeDataAsMap function") {
        assert(
          compile { (d: Data) => Builtins.unMapData(d) }
              ~=~ LamAbs(Var("d",sirData), Apply(SIRBuiltins.unMapData, Var("d",sirData), sirList(sirPair(sirData,sirData))))
        )
    }

    test("compile unsafeDataAsB function") {
        assert(
          compile { (d: Data) => Builtins.unBData(d) }
              ~=~ LamAbs(Var("d",sirData), Apply(SIRBuiltins.unBData, Var("d",sirData), sirByteString))
        )
    }

    test("compile unsafeDataAsI function") {
        assert(
          compile { (d: Data) => Builtins.unIData(d) } ~=~
              LamAbs(Var("d",sirData), Apply(SIRBuiltins.unIData, Var("d",sirData), sirInt))
        )
    }

    test("compile chooseData function") {
        assert(
          compile { (d: Data) => Builtins.chooseData[BigInt](d, 1, 2, 3, 4, 5) }
              ~=~ LamAbs(Var("d",sirData), ChooseData $ Var("d", sirData) $ 1 $ 2 $ 3 $ 4 $ 5)
        )
    }

    test("compile equalsData function") {
        assert(
          compile { (d1: Data, d2: Data) => Builtins.equalsData(d1, d2) }
              ~=~ LamAbs(Var("d1",sirData), LamAbs(Var("d2",sirData), SIRBuiltins.equalsData $ Var("d1", sirData) $ Var("d2", sirData)))
        )
    }

    test("compile serialiseData builtins") {
        assert(compile {
            Builtins.serialiseData
        } ~=~ LamAbs(Var("d",sirData), Apply(SIRBuiltins.serialiseData, Var("d",sirData), sirByteString))
        )
    }

    test("compile BLS12_381_G1 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G1_Element)
        val p2Var = Var("p2", SIRType.BLS12_381_G1_Element)
        val bsVar = Var("bs", SIRType.ByteStringPrimitive)
        val dstVar = Var("dst", SIRType.ByteStringPrimitive)
        assert(
          compile(Builtins.bls12_381_G1_add) ~=~ LamAbs(
            Var("p1", SIRType.BLS12_381_G1_Element),
            LamAbs(p2Var, Apply(
                                Apply(SIRBuiltins.bls12_381_G1_add, p1Var, Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element)),
                                p2Var,
                                SIRType.BLS12_381_G1_Element
            ))
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_neg)
              ~=~ LamAbs( Var("p", SIRType.BLS12_381_G1_Element),
            Apply(SIRBuiltins.bls12_381_G1_neg, Var("p", SIRType.BLS12_381_G1_Element), SIRType.BLS12_381_G1_Element)
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_scalarMul) == LamAbs(
            Var("s", SIRType.BLS12_381_G1_Element),
            LamAbs(Var("p",SIRType.BLS12_381_G1_Element),
                  Apply(
                      Apply(SIRBuiltins.bls12_381_G1_scalarMul, Var("s", SIRType.BLS12_381_G1_Element), Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element)),
                      Var("p", SIRType.BLS12_381_G1_Element),
                      SIRType.BLS12_381_G1_Element)
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_equal) == LamAbs(
            p1Var,
            LamAbs(p2Var, Apply(
                            Apply(SIRBuiltins.bls12_381_G1_equal, p1Var, Fun(SIRType.BLS12_381_G1_Element, SIRType.BooleanPrimitive)),
                            p2Var,
                            SIRType.BooleanPrimitive))
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_hashToGroup) == LamAbs(
            bsVar,
            LamAbs(dstVar,
                Apply(
                    Apply(SIRBuiltins.bls12_381_G1_hashToGroup, bsVar, Fun(SIRType.ByteStringPrimitive, SIRType.BLS12_381_G1_Element)),
                    dstVar,
                    SIRType.BLS12_381_G1_Element
                ))
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_compress) ~=~ LamAbs(
            Var("p", SIRType.BLS12_381_G1_Element),
            Apply(SIRBuiltins.bls12_381_G1_compress, Var("p", SIRType.BLS12_381_G1_Element), SIRType.ByteStringPrimitive)
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_uncompress) ~=~ LamAbs(
            Var("bs", SIRType.ByteStringPrimitive),
            Apply(SIRBuiltins.bls12_381_G1_uncompress, Var("bs", SIRType.ByteStringPrimitive), SIRType.BLS12_381_G1_Element)
          )
        )
    }

    test("compile BLS12_381_G2 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G2_Element)
        val p2Var = Var("p2", SIRType.BLS12_381_G2_Element)
        val pVar = Var("p", SIRType.BLS12_381_G2_Element)
        val sVar = Var("s", SIRType.BLS12_381_G2_Element)
        val bsVar = Var("bs", SIRType.ByteStringPrimitive)
        val dstVar = Var("dst", SIRType.ByteStringPrimitive)
        assert(
          compile(Builtins.bls12_381_G2_add) ~=~ LamAbs(
            p1Var,
            LamAbs(p2Var,
                Apply(
                    Apply(SIRBuiltins.bls12_381_G2_add, p1Var, Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element)),
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
            sVar,
            LamAbs(pVar,
                Apply(
                    Apply(SIRBuiltins.bls12_381_G2_scalarMul, sVar, Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element)),
                    pVar,
                    SIRType.BLS12_381_G2_Element
                )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_equal) ~=~ LamAbs(
            p1Var,
            LamAbs(p2Var,
                Apply(
                    Apply(SIRBuiltins.bls12_381_G2_equal, p1Var, Fun(SIRType.BLS12_381_G2_Element, SIRType.BooleanPrimitive)),
                    p2Var,
                    SIRType.BooleanPrimitive
                )
            )
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_hashToGroup) ~=~ LamAbs(
            bsVar,
            LamAbs(dstVar,
                Apply(
                    Apply(SIRBuiltins.bls12_381_G2_hashToGroup, bsVar, Fun(SIRType.ByteStringPrimitive, SIRType.BLS12_381_G2_Element)),
                    dstVar,
                    SIRType.BLS12_381_G2_Element
                ))
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_compress) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_compress, pVar, SIRType.ByteStringPrimitive)
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
        val p1Var = Var("p1", SIRType.BLS12_381_G1_Element)
        val p2Var = Var("p2", SIRType.BLS12_381_G2_Element)
        val r1Var = Var("r1", SIRType.BLS12_381_MlResult)
        val r2Var = Var("r2", SIRType.BLS12_381_MlResult)
        assert(
          compile(Builtins.bls12_381_millerLoop) ~=~ LamAbs(
            p1Var,
            LamAbs(p2Var,
                Apply(
                    Apply(SIRBuiltins.bls12_381_millerLoop, p1Var, Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_MlResult)),
                    p2Var,
                    SIRType.BLS12_381_MlResult
                ))
          )
        )
        assert(
          compile(Builtins.bls12_381_mulMlResult) ~=~ LamAbs(
            r1Var,
            LamAbs(r2Var,
                Apply(
                    Apply(SIRBuiltins.bls12_381_mulMlResult, r1Var, Fun(SIRType.BLS12_381_MlResult, SIRType.BLS12_381_MlResult)),
                    r2Var,
                    SIRType.BLS12_381_MlResult))
          )
        )
        assert(
          compile(Builtins.bls12_381_finalVerify) ~=~ LamAbs(
            r1Var,
            LamAbs(r2Var,
                Apply(
                    Apply(SIRBuiltins.bls12_381_finalVerify, r1Var, Fun(SIRType.BLS12_381_MlResult, SIRType.BooleanPrimitive)),
                    r2Var,
                    SIRType.BooleanPrimitive
                )
            )
          )
        )
    }

    test("compile Keccak_256 builtin") {
        val bsVar = Var("bs", SIRType.ByteStringPrimitive)
        assert(compile(Builtins.keccak_256) ~=~ LamAbs(bsVar, Apply(SIRBuiltins.keccak_256, bsVar, SIRType.ByteStringPrimitive)))
    }

    test("compile Blake2b_224 builtin") {
        val bsVar = Var("bs", SIRType.ByteStringPrimitive)
        assert(
          compile(Builtins.blake2b_224) ~=~ LamAbs(bsVar, Apply(SIRBuiltins.blake2b_224, bsVar, SIRType.ByteStringPrimitive))
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
          ) == (DefaultFun.IfThenElse $ true $ 1 $ 2)
        )
        // TODO: check if that is correct
        assert(compile(Builtins.chooseUnit()(true)) == (DefaultFun.ChooseUnit $ () $ true))
        assert(compile(Builtins.trace("dead")(BigInt(1))) == (DefaultFun.Trace $ "dead" $ 1))
    }

    test("compile Pair builtins") {
        val ftsTypeVar = TypeVar("FTS", Some(1))
        val sndTypeVar = TypeVar("SND", Some(2))
        val xTypeVar = TypeVar("X", Some(3))
        val fts = Var("fts", ftsTypeVar)
        val snd = Var("snd", sndTypeVar)
        val pv = Var("p", SIRType.Pair(sirData, sirData))
        assert(
          compile(Builtins.mkPairData) ~=~ LamAbs(
            fts,
            LamAbs(snd,
                Apply(
                    Apply(SIRBuiltins.mkPairData, fts,
                           Fun(ftsTypeVar, TypeLambda(List(xTypeVar), Fun(xTypeVar, SIRType.Pair(ftsTypeVar, xTypeVar))))
                    ),
                    snd,
                    SIRType.Pair(ftsTypeVar, sndTypeVar)
                )
            )
          )
        )
        assert(
          compile { (p: builtin.Pair[Data, Data]) =>
              builtin.Pair(Builtins.sndPair(p), Builtins.fstPair(p))
          } ~=~
              LamAbs(pv, MkPairData $ (SndPair $ pv $ (FstPair $ pv )))
        )
        assert(compile { builtin.Pair(BigInt(1), hex"deadbeef") }
             ~=~ Const(Constant.Pair(Constant.Integer(1), deadbeef), SIRType.Pair(sirInt, sirByteString))
              )
        assert(
          compile { (p: builtin.Pair[Data, Data]) => builtin.Pair(p.snd, p.fst) }
              ~=~ LamAbs(
                pv,
                Apply(
                  Apply(SIRBuiltins.mkPairData,
                       Apply(SIRBuiltins.sndPair, pv, sirData),
                       Fun(sirData, TypeLambda(List(xTypeVar), Fun(xTypeVar, SIRType.Pair(sirData, xTypeVar))))
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
                Or(And(Not(Var("a", sirBool)), sirConst(false)), sirConst(true) )
              )
        )
        // println(compiled.show)
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
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
            LamAbs(aVar, Apply(Var("check", Fun(sirBool,sirBool)), aVar, sirBool))
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
            LamAbs(aVar, Apply(Var("check", sirBool), Var("a", sirBool), sirBool))
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        assert(VM.evaluateTerm(eqterm $ true) == scalus.uplc.Term.Const(asConstant(false)))
        assert(VM.evaluateTerm(eqterm $ false) == scalus.uplc.Term.Const(asConstant(true)))
        assert(VM.evaluateTerm(neterm $ true) == scalus.uplc.Term.Const(asConstant(true)))
        assert(VM.evaluateTerm(neterm $ false) == scalus.uplc.Term.Const(asConstant(false)))
    }

    test("compile ByteString equality") {
        val eq = compile { def check(a: ByteString, b: ByteString) = a == b; check }
        val ne = compile { def check(a: ByteString, b: ByteString) = a != b; check }
        assert(
          eq ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  Var("a",sirByteString),
                  LamAbs(Var("b",sirByteString),
                      Apply(
                          Apply(SIRBuiltins.equalsByteString, Var("a",sirByteString), Fun(sirByteString, sirBool)),
                          Var("b",sirByteString),
                          sirBool
                      ))
                )
              )
            ),
            LamAbs(Var("a",sirByteString),
                LamAbs(Var("b",sirByteString),
                    Apply(
                        Apply(Var("check",Fun(sirByteString,sirByteString)), Var("a",sirByteString), Fun(sirByteString,sirBool)),
                        Var("b",sirByteString),
                        sirBool
            )))
          )
        )
        assert(
          ne ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  Var("a",sirByteString),
                  LamAbs(Var("b",sirByteString),
                      Not(Apply(
                          Apply(SIRBuiltins.equalsByteString, Var("a",sirByteString), Fun(sirByteString, sirBool)),
                          Var("b",sirByteString),
                          sirBool
                      )))
                )
              )
            ),
            LamAbs(Var("a",sirByteString),
                LamAbs(Var("b", sirByteString),
                    Apply(
                        Apply(Var("check",Fun(sirByteString,Fun(sirByteString,sirBool))),
                              Var("a", sirByteString),
                              Fun(sirByteString,sirBool)
                        ),
                        Var("b", sirByteString),
                        sirBool
                    )))
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        assert(
          VM.evaluateTerm(eqterm $ ByteString.empty $ ByteString.empty) == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          VM.evaluateTerm(eqterm $ ByteString.empty $ hex"deadbeef") == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          VM.evaluateTerm(neterm $ ByteString.empty $ ByteString.empty) == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          VM.evaluateTerm(neterm $ ByteString.empty $ hex"deadbeef") == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
    }

    test("compile String equality") {
        val eq = compile { def check(a: String, b: String) = a == b; check }
        val ne = compile { def check(a: String, b: String) = a != b; check }
        val aVar = Var("a", sirString)
        val bVar = Var("b", sirString)
        assert(
          eq == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  aVar,
                  LamAbs(bVar,
                      Apply(
                        Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool)),
                        bVar,
                        sirBool
                      ))
                )
              )
            ),
            LamAbs(aVar, LamAbs(bVar,
                Apply(
                    Apply(
                        Var("check", Fun(sirString, Fun(sirString, sirBool))),
                        aVar,
                        Fun(sirString, sirBool)
                    ),
                    bVar,
                    sirBool
                )))
          )
        )
        assert(
          ne == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  aVar,
                  LamAbs(bVar,
                      Not(Apply(
                          Apply(SIRBuiltins.equalsString, aVar, Fun(sirString,sirBool) ),
                          aVar,
                          sirBool)))
                )
              )
            ),
            LamAbs(aVar,
                LamAbs(bVar,
                    Apply(
                        Apply(Var("check", Fun(sirString,Fun(sirString,sirBool))), aVar, Fun(sirString,sirBool) ),
                        bVar,
                        sirBool)))
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        assert(
          VM.evaluateTerm(eqterm $ "" $ "") == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          VM.evaluateTerm(eqterm $ "" $ "deadbeef") == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          VM.evaluateTerm(neterm $ "" $ "") == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          VM.evaluateTerm(neterm $ "" $ "deadbeef") == scalus.uplc.Term.Const(
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
                  LamAbs(b, Apply(Apply(SIRBuiltins.equalsData, a, Fun(sirData,sirBool) ), b, sirBool ))
                )
              )
            ),
            LamAbs(a, LamAbs(b, Apply(Apply(check, a, Fun(sirData,sirBool)), b, sirBool)))
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
                  LamAbs(b, Not(Apply(Apply(SIRBuiltins.equalsData, a, Fun(sirData,sirBool)), b, sirBool )))
                )
              )
            ),
            LamAbs(a, LamAbs(b, Apply(Apply(check, a, Fun(sirData,sirBool)), b, sirBool )))
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.uplc.TermDSL.{*, given}
        import scalus.builtin.Data.toData
        import scalus.builtin.ToDataInstances.given

        assert(
          VM.evaluateTerm(eqterm $ 1.toData $ 1.toData) == scalus.uplc.Term.Const(
            asConstant(true)
          )
        )
        assert(
          VM.evaluateTerm(eqterm $ "".toData $ "deadbeef".toData) == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          VM.evaluateTerm(neterm $ 1.toData $ 1.toData) == scalus.uplc.Term.Const(
            asConstant(false)
          )
        )
        assert(
          VM.evaluateTerm(neterm $ "".toData $ "deadbeef".toData) == scalus.uplc.Term.Const(
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
        val term = compiled.toUplc()
        // println(term.show)
        val evaled = VM.evaluateTerm(term)
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

    test("compile datatypes") {
        import scalus.ledger.api.v1.PubKeyHash
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh.hash
        }

        val pkhType =
            DataDecl("PubKeyHash",
                List(ConstrDecl("PubKeyHash",  SIRVarStorage.DEFAULT, List(TypeBinding("hash", sirByteString)), List.empty)),
                List.empty
            ).tp

        assert(
          compiled ~=~
              Decl(
                DataDecl("PubKeyHash",
                    List(ConstrDecl("PubKeyHash", SIRVarStorage.DEFAULT, List(TypeBinding("hash", sirByteString)), List.empty)),
                    List.empty),
                Let(
                  NonRec,
                  List(
                    Binding(
                      "pkh",
                      Constr(
                        "PubKeyHash",
                        DataDecl("PubKeyHash", List(ConstrDecl("PubKeyHash", SIRVarStorage.DEFAULT, List(TypeBinding("hash", sirByteString)), List.empty)), List.empty),
                        List(Const(uplc.Constant.ByteString(hex"DEADBEEF"), sirByteString))
                      )
                    )
                  ),
                  Apply(Var("pkh",pkhType), LamAbs(Var("hash", sirByteString), Var("hash", sirByteString)), sirByteString)
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
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(false)))
    }

    test("compile match on a case class") {
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh match
                case PubKeyHash(hash) => hash
        }
        // println(compiled.show)
        val term = compiled.toUplc()
        // println(term.show)
        val evaled = VM.evaluateTerm(term)
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
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
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
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
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
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
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
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
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
        import scalus.uplc.TermDSL.{*, given}
        import scalus.builtin.Data.{*}
        import DefaultUni.asConstant
        val appliedScript = Program(version = (1, 0, 0), term = term $ scriptContext.toData)
        val r @ Result.Success(evaled, budget, costs, logs) =
            VM.evaluateDebug(appliedScript.term): @unchecked
        assert(evaled == scalus.uplc.Term.Const(asConstant(hex"deadbeef")))
        val flatBytesLength = appliedScript.flatEncoded.length
        // println(Utils.bytesToHex(flatBytes))
        assert(flatBytesLength == 332)
    }

    test("@Ignore annotation") {
        @Ignore
        def foo() = 1

        assert(compile {
            @Ignore val a = true

            @Ignore def foo() = true
        } == Const(Constant.Unit, SIRType.VoidPrimitive))
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
        val term = compiled.toUplc()
        VM.evaluateDebug(term) match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Bool(false)))
                assert(logs == List("oneEqualsTwo ? False: { mem: 0.002334, cpu: 0.539980 }"))
            case Result.Failure(exception, _, _, _) => fail(exception)
    }





