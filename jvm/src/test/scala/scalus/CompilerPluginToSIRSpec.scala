package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.{compile, fieldAsData}
import scalus.builtin.ByteString.*
import scalus.builtin.{Builtins, ByteString, Data, given}
import scalus.ledger.api.v1.*
import scalus.prelude.List.{Cons, Nil}
import scalus.prelude.Prelude.given
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.SIRType.{Boolean, Fun, TypeVar}
import scalus.sir.SirDSL.{*, given}
import scalus.sir.*
import scalus.uplc.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.eval.{PlutusVM, Result}

import scala.collection.immutable
import scala.language.implicitConversions

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

    def sirConst(x: Int) = Const(Constant.Integer(x), SIRType.Integer, AnnotationsDecl.empty)
    def sirConst(x: BigInt) = Const(Constant.Integer(x), SIRType.Integer, AnnotationsDecl.empty)
    def sirConst(x: Boolean) = Const(Constant.Bool(x), SIRType.Boolean, AnnotationsDecl.empty)
    def sirConst(x: String) = Const(Constant.String(x), SIRType.String, AnnotationsDecl.empty)
    def sirConst(x: ByteString) =
        Const(Constant.ByteString(x), SIRType.ByteString, AnnotationsDecl.empty)
    def sirConst(x: Data) = Const(Constant.Data(x), SIRType.Data, AnnotationsDecl.empty)
    def sirConstUnit = Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty)

    def AnE = AnnotationsDecl.empty

    test("compile literals") {
        assert(
          compile(false) ~=~ Const(Constant.Bool(false), SIRType.Boolean, AnnotationsDecl.empty)
        )
        assert(compile(true) ~=~ Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty))
        assert(compile(()) ~=~ Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty))
        assert(
          compile("foo") ~=~ Const(Constant.String("foo"), SIRType.String, AnnotationsDecl.empty)
        )
        assert(
          compile(BigInt("15511210043330985984000000")) ~=~ Const(
            Constant.Integer(BigInt("15511210043330985984000000")),
            SIRType.Integer,
            AnnotationsDecl.empty
          )
        )
        assert(
          compile(12: BigInt) ~=~ Const(
            Constant.Integer(BigInt("12")),
            SIRType.Integer,
            AnnotationsDecl.empty
          )
        )
        assert(
          compile(scala.math.BigInt.int2bigInt(12)) ~=~ Const(
            Constant.Integer(BigInt("12")),
            SIRType.Integer,
            AnnotationsDecl.empty
          )
        )

        // ByteStrings
        assert(
          compile(builtin.ByteString.empty) ~=~ Const(
            Constant.ByteString(builtin.ByteString.empty),
            SIRType.ByteString,
            AnnotationsDecl.empty
          )
        )

        assert(
          compile(builtin.ByteString.fromHex("deadbeef")) ~=~ Const(
            deadbeef,
            SIRType.ByteString,
            AnnotationsDecl.empty
          )
        )
        assert(
          compile(hex"deadbeef") ~=~ Const(deadbeef, SIRType.ByteString, AnnotationsDecl.empty)
        )
        assert(
          compile(builtin.ByteString.fromString("deadbeef")) ~=~ Const(
            Constant.ByteString(builtin.ByteString.fromString("deadbeef")),
            SIRType.ByteString,
            AnnotationsDecl.empty
          )
        )
    }

    test("compile if-then-else") {
        assert(
          compile {
              if Builtins.equalsInteger(1, 2) then () else ()
          } ~=~ SIR.IfThenElse(
            Apply(
              Apply(
                SIRBuiltins.equalsInteger,
                Const(Constant.Integer(1), SIRType.Integer, AnnotationsDecl.empty),
                SIRType.Fun(SIRType.Integer, SIRType.Boolean),
                AnnotationsDecl.empty
              ),
              Const(Constant.Integer(2), SIRType.Integer, AnnotationsDecl.empty),
              SIRType.Boolean,
              AnnotationsDecl.empty
            ),
            Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty),
            Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty),
            SIRType.Unit,
            AnnotationsDecl.empty
          )
        )
    }

    test("compile val def") {
        assert(
          compile {
              val a = true
              a
          } ~=~ Let(
            Recursivity.NonRec,
            immutable.List(
              Binding("a", Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty))
            ),
            Var("a", SIRType.Boolean, AnnotationsDecl.empty),
            AnnotationsDecl.empty
          )
        )
    }

    test("compile def") {
        assert(
          compile {
              def b() = true

              def c(x: Boolean) = x

              c(b())
          } ~=~ Let(
            Recursivity.Rec,
            immutable.List(
              Binding(
                "b",
                LamAbs(
                  Var("_", SIRType.Unit, AnE),
                  Const(Constant.Bool(true), SIRType.Boolean, AnE),
                  AnE
                )
              )
            ),
            Let(
              Recursivity.Rec,
              immutable.List(
                Binding(
                  "c",
                  LamAbs(Var("x", SIRType.Boolean, AnE), Var("x", SIRType.Boolean, AnE), AnE)
                )
              ),
              Apply(
                Var("c", SIRType.Fun(Boolean, SIRType.Boolean), AnE),
                Apply(
                  Var("b", SIRType.Fun(SIRType.Unit, SIRType.Boolean), AnE),
                  Const(Constant.Unit, SIRType.Unit, AnE),
                  SIRType.Boolean,
                  AnE
                ),
                SIRType.Boolean,
                AnE
              ),
              AnE
            ),
            AnE
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
          Var("tail", tailType, AnE),
          LamAbs(
            Var("ctx", listData, AnE),
            Apply(Var("tail", tailType, AnE), Var("ctx", listData, AnE), listData, AnE),
            AnE
          ),
          AnE
        )

        // SIRUnify.unifyType(sir.asInstanceOf[SIRExpr].tp, constructedExpr.tp, SIRUnify.Env.empty.copy(debug = true)) match {
        //    case success@SIRUnify.UnificationSuccess(env,tp) => println("unifyType success")
        //    case failure@SIRUnify.UnificationFailure(path, left, right) => println(s"unifyType failure: ${failure}")
        // }

        assert(
          sir ~=~ LamAbs(
            Var("tail", tailType, AnE),
            LamAbs(
              Var("ctx", listData, AnE),
              Apply(Var("tail", tailType, AnE), Var("ctx", listData, AnE), listData, AnE),
              AnE
            ),
            AnE
          )
        )

    }

    test("compile inline def") {
        assert(
          compile {
              inline def b = true

              b
          } ~=~ Const(Constant.Bool(true), sirBool, AnE)
        )
    }

    test("compile lambda") {
        assert(
          compile {
              val a = (x: Boolean) => x
              a(true)
          } ~=~ Let(
            NonRec,
            List(Binding("a", LamAbs(Var("x", sirBool, AnE), Var("x", sirBool, AnE), AnE))),
            Apply(
              Var("a", SIRType.Fun(sirBool, sirBool), AnE),
              Const(Constant.Bool(true), sirBool, AnE),
              sirBool,
              AnE
            ),
            AnE
          )
        )
    }

    test("compile throw") {
        assert(compile {
            throw new RuntimeException("foo")
        } ~=~ Error("foo", AnE))
    }

    test("compile ToData") {
        import scalus.builtin.Data.*
        import scalus.builtin.ToDataInstances.given
        val compiled = compile {
            BigInt(1).toData
        }
        val expected = Let(
          Rec,
          immutable.List(
            Binding(
              "scalus.builtin.ToDataInstances$.given_ToData_BigInt",
              LamAbs(
                Var("a", sirInt, AnE),
                Apply(SIRBuiltins.iData, Var("a", sirInt, AnE), sirData, AnE),
                AnE
              )
            )
          ),
          Let(
            NonRec,
            immutable.List(Binding("a$proxy1", Const(Constant.Integer(1), sirInt, AnE))),
            Apply(
              ExternalVar(
                "scalus.builtin.ToDataInstances$",
                "scalus.builtin.ToDataInstances$.given_ToData_BigInt",
                Fun(sirInt, sirData),
                AnE
              ),
              Var("a$proxy1", sirInt, AnE),
              sirData,
              AnE
            ),
            AnE
          ),
          AnE
        )
        assert(compiled ~=~ expected)
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
            SIRType.List(SIRType.Integer),
            AnE
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
            SIRType.List(SIRType.Integer),
            AnE
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
          List(Binding("a", Const(Constant.String("foo"), sirString, AnE))),
          Apply(
            Apply(
              SIRBuiltins.mkCons,
              Const(Constant.String("bar"), sirString, AnE),
              Fun(SIRType.List(sirString), SIRType.List(sirString)),
              AnE
            ),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Var("a", sirString, AnE),
                Fun(SIRType.List(sirString), SIRType.List(sirString)),
                AnE
              ),
              Const(Constant.List(DefaultUni.String, List()), SIRType.List(sirString), AnE),
              SIRType.List(sirString),
              AnE
            ),
            SIRType.List(sirString),
            AnE
          ),
          AnE
        )

        assert(
          compiled ~=~ expected
        )
    }

    test("compile head function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.head }
              ~=~ LamAbs(
                Var("l", sirList(sirInt), AnE),
                Apply(SIRBuiltins.headList, Var("l", sirList(sirInt), AnE), sirInt, AnE),
                AnE
              )
        )
    }

    test("compile tail function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.tail }
              ~=~ LamAbs(
                Var("l", sirList(sirInt), AnE),
                Apply(SIRBuiltins.tailList, Var("l", sirList(sirInt), AnE), sirList(sirInt), AnE),
                AnE
              )
        )
    }

    test("compile isEmpty function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.isEmpty }
              ~=~ LamAbs(
                Var("l", sirList(sirInt), AnE),
                Apply(SIRBuiltins.nullList, Var("l", sirList(sirInt), AnE), sirBool, AnE),
                AnE
              )
        )
    }

    test("compile mkNilData") {
        assert(
          compile(Builtins.mkNilData())
              ~=~
                  (Apply(
                    SIRBuiltins.mkNilData,
                    Const(Constant.Unit, sirVoid, AnE),
                    sirList(sirData),
                    AnE
                  ))
        )
    }

    test("compile mkNilPairData") {

        assert(
          compile(Builtins.mkNilPairData())
              ~=~
                  (Apply(
                    SIRBuiltins.mkNilPairData,
                    Const(Constant.Unit, sirVoid, AnE),
                    SIRType.List(SIRType.Pair(sirData, sirData)),
                    AnE
                  ))
        )
    }

    test("compile mkConstr builtins") {
        val nilData =
            Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.List(sirData), AnE)
        assert(
          compile(
            Builtins.constrData(
              1,
              builtin.List(Builtins.iData(2))
            )
          ) ~=~ Apply(
            Apply(SIRBuiltins.constrData, sirConst(1), Fun(sirList(sirData), sirData), AnE),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Apply(SIRBuiltins.iData, sirConst(2), sirData, AnE),
                Fun(sirList(sirData), sirList(sirData)),
                AnE
              ),
              nilData,
              sirList(sirData),
              AnE
            ),
            sirData,
            AnE
          )
        )
    }

    test("compile mkList builtins") {
        val nilData =
            Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.List(sirData), AnE)
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
                        Apply(SIRBuiltins.iData, sirConst(1), sirData, AnE),
                        Fun(sirList(sirData), sirList(sirData)),
                        AnE
                      ),
                      nilData,
                      sirList(sirData),
                      AnE
                    ),
                    sirData,
                    AnE
                  )
        )
    }

    test("compile mkPairData builtins") {

        val compiled = compile(builtin.Pair(Builtins.bData(hex"deadbeef"), Builtins.iData(1)))

        val expected =
            Apply(
              Apply(
                SIRBuiltins.mkPairData,
                Apply(SIRBuiltins.bData, sirConst(hex"deadbeef"), sirData, AnE),
                Fun(sirData, sirPair(sirData, sirData)),
                AnE
              ),
              Apply(SIRBuiltins.iData, sirConst(1), sirData, AnE),
              sirPair(sirData, sirData),
              AnE
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
                    Apply(SIRBuiltins.bData, Const(deadbeef, sirByteString, AnE), sirData, AnE),
                    Fun(sirData, sirPair(sirData, sirData)),
                    AnE
                  ),
                  Apply(SIRBuiltins.iData, sirConst(1), sirData, AnE),
                  sirPair(sirData, sirData),
                  AnE
                ),
                Fun(sirList(sirPair(sirData, sirData)), sirList(sirPair(sirData, sirData))),
                AnE
              ),
              Const(
                Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), immutable.Nil),
                SIRType.List(sirPair(sirData, sirData)),
                AnE
              ),
              sirList(sirPair(sirData, sirData)),
              AnE
            ),
            sirData,
            AnE
          )
        )
    }

    test("compile unsafeDataAsConstr function") {
        assert(
          compile { (d: Data) => Builtins.unConstrData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(
                  SIRBuiltins.unConstrData,
                  Var("d", sirData, AnE),
                  sirPair(sirInt, sirList(sirData)),
                  AnE
                ),
                AnE
              )
        )
    }

    test("compile unsafeDataAsList function") {
        assert(
          compile { (d: Data) => Builtins.unListData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unListData, Var("d", sirData, AnE), sirList(sirData), AnE),
                AnE
              )
        )
    }

    test("compile unsafeDataAsMap function") {
        assert(
          compile { (d: Data) => Builtins.unMapData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(
                  SIRBuiltins.unMapData,
                  Var("d", sirData, AnE),
                  sirList(sirPair(sirData, sirData)),
                  AnE
                ),
                AnE
              )
        )
    }

    test("compile unsafeDataAsB function") {
        assert(
          compile { (d: Data) => Builtins.unBData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unBData, Var("d", sirData, AnE), sirByteString, AnE),
                AnE
              )
        )
    }

    test("compile unsafeDataAsI function") {
        assert(
          compile { (d: Data) => Builtins.unIData(d) } ~=~
              LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unIData, Var("d", sirData, AnE), sirInt, AnE),
                AnE
              )
        )
    }

    test("compile chooseData function") {
        assert(
          compile { (d: Data) => Builtins.chooseData[BigInt](d, 1, 2, 3, 4, 5) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                ChooseData $ Var("d", sirData, AnE) $ 1 $ 2 $ 3 $ 4 $ 5,
                AnE
              )
        )
    }

    test("compile equalsData function") {
        assert(
          compile { (d1: Data, d2: Data) => Builtins.equalsData(d1, d2) }
              ~=~ LamAbs(
                Var("d1", sirData, AnE),
                LamAbs(
                  Var("d2", sirData, AnE),
                  SIRBuiltins.equalsData $ Var("d1", sirData, AnE) $ Var("d2", sirData, AnE),
                  AnE
                ),
                AnE
              )
        )
    }

    test("compile serialiseData builtins") {
        assert(
          compile {
              Builtins.serialiseData
          } ~=~ LamAbs(
            Var("d", sirData, AnE),
            Apply(SIRBuiltins.serialiseData, Var("d", sirData, AnE), sirByteString, AnE),
            AnE
          )
        )
    }

    test("compile BLS12_381_G1 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G1_Element, AnE)
        val p2Var = Var("p2", SIRType.BLS12_381_G1_Element, AnE)
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        val dstVar = Var("dst", SIRType.ByteString, AnE)

        assert(
          compile(Builtins.bls12_381_G1_add) ~=~ LamAbs(
            Var("p1", SIRType.BLS12_381_G1_Element, AnE),
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_add,
                  p1Var,
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element),
                  AnE
                ),
                p2Var,
                SIRType.BLS12_381_G1_Element,
                AnE
              ),
              AnE
            ),
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_neg)
              ~=~ LamAbs(
                Var("p", SIRType.BLS12_381_G1_Element, AnE),
                Apply(
                  SIRBuiltins.bls12_381_G1_neg,
                  Var("p", SIRType.BLS12_381_G1_Element, AnE),
                  SIRType.BLS12_381_G1_Element,
                  AnE
                ),
                AnE
              )
        )
        assert(
          compile(Builtins.bls12_381_G1_scalarMul) == LamAbs(
            Var("s", SIRType.Integer, AnE),
            LamAbs(
              Var("p", SIRType.BLS12_381_G1_Element, AnE),
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G1_scalarMul,
                  Var("s", SIRType.Integer, AnE),
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.BLS12_381_G1_Element),
                  AnE
                ),
                Var("p", SIRType.BLS12_381_G1_Element, AnE),
                SIRType.BLS12_381_G1_Element,
                AnE
              ),
              AnE
            ),
            AnE
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
                  Fun(SIRType.BLS12_381_G1_Element, SIRType.Boolean),
                  AnE
                ),
                p2Var,
                SIRType.Boolean,
                AnE
              ),
              AnE
            ),
            AnE
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
                  Fun(SIRType.ByteString, SIRType.BLS12_381_G1_Element),
                  AnE
                ),
                dstVar,
                SIRType.BLS12_381_G1_Element,
                AnE
              ),
              AnE
            ),
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_compress) ~=~ LamAbs(
            Var("p", SIRType.BLS12_381_G1_Element, AnE),
            Apply(
              SIRBuiltins.bls12_381_G1_compress,
              Var("p", SIRType.BLS12_381_G1_Element, AnE),
              SIRType.ByteString,
              AnE
            ),
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G1_uncompress) ~=~ LamAbs(
            Var("bs", SIRType.ByteString, AnE),
            Apply(
              SIRBuiltins.bls12_381_G1_uncompress,
              Var("bs", SIRType.ByteString, AnE),
              SIRType.BLS12_381_G1_Element,
              AnE
            ),
            AnE
          )
        )
    }

    test("compile BLS12_381_G2 builtins") {
        val p1Var = Var("p1", SIRType.BLS12_381_G2_Element, AnE)
        val p2Var = Var("p2", SIRType.BLS12_381_G2_Element, AnE)
        val pVar = Var("p", SIRType.BLS12_381_G2_Element, AnE)
        val sBlsVar = Var("s", SIRType.BLS12_381_G2_Element, AnE)
        val sIntVar = Var("s", SIRType.Integer, AnE)
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        val dstVar = Var("dst", SIRType.ByteString, AnE)
        assert(
          compile(Builtins.bls12_381_G2_add) ~=~ LamAbs(
            p1Var,
            LamAbs(
              p2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_G2_add,
                  p1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element),
                  AnE
                ),
                p2Var,
                SIRType.BLS12_381_G2_Element,
                AnE
              ),
              AnE
            ),
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_neg) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_neg, pVar, SIRType.BLS12_381_G2_Element, AnE),
            AnE
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
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_G2_Element),
                  AnE
                ),
                pVar,
                SIRType.BLS12_381_G2_Element,
                AnE
              ),
              AnE
            ),
            AnE
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
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.Boolean),
                  AnE
                ),
                p2Var,
                SIRType.Boolean,
                AnE
              ),
              AnE
            ),
            AnE
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
                  Fun(SIRType.ByteString, SIRType.BLS12_381_G2_Element),
                  AnE
                ),
                dstVar,
                SIRType.BLS12_381_G2_Element,
                AnE
              ),
              AnE
            ),
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_compress) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_compress, pVar, SIRType.ByteString, AnE),
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_uncompress) == LamAbs(
            bsVar,
            Apply(SIRBuiltins.bls12_381_G2_uncompress, bsVar, SIRType.BLS12_381_G2_Element, AnE),
            AnE
          )
        )
    }

    test("compile BLS12_381 pairing operations builtins") {
        val p1BlsG1Var = Var("p1", SIRType.BLS12_381_G1_Element, AnE)
        val p2BlsG2Var = Var("p2", SIRType.BLS12_381_G2_Element, AnE)
        val p1BlsMlVar = Var("p1", SIRType.BLS12_381_MlResult, AnE)
        val p2BlsMlVar = Var("p2", SIRType.BLS12_381_MlResult, AnE)
        val r1Var = Var("r1", SIRType.BLS12_381_MlResult, AnE)
        val r2Var = Var("r2", SIRType.BLS12_381_MlResult, AnE)

        assert(
          compile(Builtins.bls12_381_millerLoop) ~=~ LamAbs(
            p1BlsG1Var,
            LamAbs(
              p2BlsG2Var,
              Apply(
                Apply(
                  SIRBuiltins.bls12_381_millerLoop,
                  p1BlsG1Var,
                  Fun(SIRType.BLS12_381_G2_Element, SIRType.BLS12_381_MlResult),
                  AnE
                ),
                p2BlsG2Var,
                SIRType.BLS12_381_MlResult,
                AnE
              ),
              AnE
            ),
            AnE
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
                  Fun(SIRType.BLS12_381_MlResult, SIRType.BLS12_381_MlResult),
                  AnE
                ),
                r2Var,
                SIRType.BLS12_381_MlResult,
                AnE
              ),
              AnE
            ),
            AnE
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
                  Fun(SIRType.BLS12_381_MlResult, SIRType.Boolean),
                  AnE
                ),
                p2BlsMlVar,
                SIRType.Boolean,
                AnE
              ),
              AnE
            ),
            AnE
          )
        )
    }

    test("compile Keccak_256 builtin") {
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        assert(
          compile(Builtins.keccak_256) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.keccak_256, bsVar, SIRType.ByteString, AnE),
            AnE
          )
        )
    }

    test("compile Blake2b_224 builtin") {
        val bsVar = Var("bs", SIRType.ByteString, AnE)
        assert(
          compile(Builtins.blake2b_224) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.blake2b_224, bsVar, SIRType.ByteString, AnE),
            AnE
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
        val fst = Var("fst", sirData, AnE)
        val snd = Var("snd", sirData, AnE)
        val pv = Var("p", SIRType.Pair(sirData, sirData), AnE)

        assert(
          compile(Builtins.mkPairData) ~=~ LamAbs(
            fst,
            LamAbs(
              snd,
              Apply(
                Apply(
                  SIRBuiltins.mkPairData,
                  fst,
                  Fun(sirData, SIRType.Pair(sirData, sirData)),
                  AnE
                ),
                snd,
                SIRType.Pair(sirData, sirData),
                AnE
              ),
              AnE
            ),
            AnE
          )
        )

        assert(
          compile { (p: builtin.Pair[Data, Data]) =>
              builtin.Pair(Builtins.sndPair(p), Builtins.fstPair(p))
          } ~=~
              LamAbs(
                pv,
                (SIRBuiltins.mkPairData $ (SIRBuiltins.sndPair $ pv)) $ (SIRBuiltins.fstPair $ pv),
                AnE
              )
        )

        assert(
          compile { builtin.Pair(BigInt(1), hex"deadbeef") }
              ~=~ Const(
                Constant.Pair(Constant.Integer(1), deadbeef),
                SIRType.Pair(sirInt, sirByteString),
                AnE
              )
        )

        assert(
          compile { (p: builtin.Pair[Data, Data]) => builtin.Pair(p.snd, p.fst) }
              ~=~ LamAbs(
                pv,
                Apply(
                  Apply(
                    SIRBuiltins.mkPairData,
                    Apply(SIRBuiltins.sndPair, pv, sirData, AnE),
                    Fun(sirData, SIRType.Pair(sirData, sirData)),
                    AnE
                  ),
                  Apply(SIRBuiltins.fstPair, pv, sirData, AnE),
                  SIRType.Pair(sirData, sirData),
                  AnE
                ),
                AnE
              )
        )

    }

    test("compile Boolean &&, ||, ! builtins") {
        val compiled = compile {
            val a = true || (throw new Exception("M"))
            !a && false || true
        }
        assert(
          compiled ~=~
              Let(
                NonRec,
                List(Binding("a", Or(sirConst(true), Error("M", AnE)))),
                Or(And(Not(Var("a", sirBool, AnE)), sirConst(false)), sirConst(true)),
                AnE
              )
        )
        // println(compiled.show)
        val evaled = compiled.toUplc().evaluate
        // println(evaled.show)
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
    }

    test("compile Boolean equality") {
        val eq = compile { def check(a: Boolean) = a == false; check }
        val ne = compile { def check(a: Boolean) = a != false; check }
        val aVar = Var("a", sirBool, AnE)

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
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool, AnE),
                    sirBool,
                    AnE
                  ),
                  AnE
                )
              )
            ),
            LamAbs(aVar, Apply(Var("check", Fun(sirBool, sirBool), AnE), aVar, sirBool, AnE), AnE),
            AnE
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
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool, AnE),
                    sirConst(false),
                    sirBool,
                    AnE
                  ),
                  AnE
                )
              )
            ),
            LamAbs(aVar, Apply(Var("check", Fun(sirBool, sirBool), AnE), aVar, sirBool, AnE), AnE),
            AnE
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
                Var("a", sirByteString, AnE),
                LamAbs(
                  Var("b", sirByteString, AnE),
                  Apply(
                    Apply(
                      SIRBuiltins.equalsByteString,
                      Var("a", sirByteString, AnE),
                      Fun(sirByteString, sirBool),
                      AnE
                    ),
                    Var("b", sirByteString, AnE),
                    sirBool,
                    AnE
                  ),
                  AnE
                ),
                AnE
              )
            )
          ),
          LamAbs(
            Var("a", sirByteString, AnE),
            LamAbs(
              Var("b", sirByteString, AnE),
              Apply(
                Apply(
                  Var("check", Fun(sirByteString, Fun(sirByteString, sirBool)), AnE),
                  Var("a", sirByteString, AnE),
                  Fun(sirByteString, sirBool),
                  AnE
                ),
                Var("b", sirByteString, AnE),
                sirBool,
                AnE
              ),
              AnE
            ),
            AnE
          ),
          AnE
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
                  Var("a", sirByteString, AnE),
                  LamAbs(
                    Var("b", sirByteString, AnE),
                    Not(
                      Apply(
                        Apply(
                          SIRBuiltins.equalsByteString,
                          Var("a", sirByteString, AnE),
                          Fun(sirByteString, sirBool),
                          AnE
                        ),
                        Var("b", sirByteString, AnE),
                        sirBool,
                        AnE
                      )
                    ),
                    AnE
                  ),
                  AnE
                )
              )
            ),
            LamAbs(
              Var("a", sirByteString, AnE),
              LamAbs(
                Var("b", sirByteString, AnE),
                Apply(
                  Apply(
                    Var("check", Fun(sirByteString, Fun(sirByteString, sirBool)), AnE),
                    Var("a", sirByteString, AnE),
                    Fun(sirByteString, sirBool),
                    AnE
                  ),
                  Var("b", sirByteString, AnE),
                  sirBool,
                  AnE
                ),
                AnE
              ),
              AnE
            ),
            AnE
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

        val aVar = Var("a", sirString, AnE)
        val bVar = Var("b", sirString, AnE)

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
                      Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool), AnE),
                      bVar,
                      sirBool,
                      AnE
                    ),
                    AnE
                  ),
                  AnE
                )
              )
            ),
            LamAbs(
              aVar,
              LamAbs(
                bVar,
                Apply(
                  Apply(
                    Var("check", Fun(sirString, Fun(sirString, sirBool)), AnE),
                    aVar,
                    Fun(sirString, sirBool),
                    AnE
                  ),
                  bVar,
                  sirBool,
                  AnE
                ),
                AnE
              ),
              AnE
            ),
            AnE
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
                      Apply(SIRBuiltins.equalsString, aVar, Fun(sirString, sirBool), AnE),
                      bVar,
                      sirBool,
                      AnE
                    )
                  ),
                  AnE
                ),
                AnE
              )
            )
          ),
          LamAbs(
            aVar,
            LamAbs(
              bVar,
              Apply(
                Apply(
                  Var("check", Fun(sirString, Fun(sirString, sirBool)), AnE),
                  aVar,
                  Fun(sirString, sirBool),
                  AnE
                ),
                bVar,
                sirBool,
                AnE
              ),
              AnE
            ),
            AnE
          ),
          AnE
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
        val a = Var("a", sirData, AnE)
        val b = Var("b", sirData, AnE)
        val check = Var("check", Fun(sirData, Fun(sirData, sirBool)), AnE)
        assert(
          eq ~=~ Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  a,
                  LamAbs(
                    b,
                    Apply(
                      Apply(SIRBuiltins.equalsData, a, Fun(sirData, sirBool), AnE),
                      b,
                      sirBool,
                      AnE
                    ),
                    AnE
                  ),
                  AnE
                )
              )
            ),
            LamAbs(
              a,
              LamAbs(b, Apply(Apply(check, a, Fun(sirData, sirBool), AnE), b, sirBool, AnE), AnE),
              AnE
            ),
            AnE
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
                    Not(
                      Apply(
                        Apply(SIRBuiltins.equalsData, a, Fun(sirData, sirBool), AnE),
                        b,
                        sirBool,
                        AnE
                      )
                    ),
                    AnE
                  ),
                  AnE
                )
              )
            ),
            LamAbs(
              a,
              LamAbs(b, Apply(Apply(check, a, Fun(sirData, sirBool), AnE), b, sirBool, AnE), AnE),
              AnE
            ),
            AnE
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.builtin.Data.toData
        import scalus.builtin.ToDataInstances.given
        import scalus.uplc.TermDSL.{*, given}

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

    private val pubKeyHashDataDecl = DataDecl(
      "scalus.ledger.api.v1.PubKeyHash",
      List(
        ConstrDecl(
          "scalus.ledger.api.v1.PubKeyHash",
          SIRVarStorage.DEFAULT,
          List(TypeBinding("hash", sirByteString)),
          List.empty,
          List.empty,
          AnE
        )
      ),
      List.empty,
      AnE
    )

    test("compile datatypes") {
        import scalus.ledger.api.v1.PubKeyHash
        val compiled = compile {
            val pkh = new scalus.ledger.api.v1.PubKeyHash(hex"deadbeef")
            pkh.hash
        }

        val expected = Decl(
          pubKeyHashDataDecl,
          Let(
            NonRec,
            List(
              Binding(
                "pkh",
                Constr(
                  "scalus.ledger.api.v1.PubKeyHash",
                  pubKeyHashDataDecl,
                  List(Const(uplc.Constant.ByteString(hex"DEADBEEF"), sirByteString, AnE)),
                  pubKeyHashDataDecl.constrType("scalus.ledger.api.v1.PubKeyHash"),
                  AnE
                )
              )
            ),
            Select(
              Var("pkh", pubKeyHashDataDecl.constrType("scalus.ledger.api.v1.PubKeyHash"), AnE),
              "hash",
              sirByteString,
              AnE
            ),
            AnE
          )
        )

        // SIRUnify.unifySIR(compiled, expected, SIRUnify.Env.empty.copy(debug = true)) match
        //    case SIRUnify.UnificationSuccess(env, unificator) =>
        //    case SIRUnify.UnificationFailure(path,left,right) =>
        //        if left.isInstanceOf[SIR] then
        //            println(s"compile datatypes: unify left:\n${left.asInstanceOf[SIR].show}")
        //        if right.isInstanceOf[SIR] then
        //            println(s"compile datatypes: unify right:\n${right.asInstanceOf[SIR].show}")

        assert(
          compiled ~=~ expected
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
                  "scalus.ledger.api.v1.PubKeyHash",
                  pubKeyHashDataDecl,
                  List(Const(Constant.ByteString(hex"deadbeef"), SIRType.ByteString, AnE)),
                  SIRType.CaseClass(pubKeyHashDataDecl.constructors.head, scala.Nil, None),
                  AnE
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
        val uplc = compiled.toUplc()
        val evaled = uplc.evaluate
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
        import DefaultUni.asConstant
        import scalus.builtin.Data.*
        import scalus.uplc.TermDSL.given
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
        } == Const(Constant.Unit, SIRType.Unit, AnE))
    }

    test("Ignore PlatformSpecific arguments") {
        // Make sure that the implicit PlatformSpecific argument is not generated
        assert(compile(Builtins.sha2_256) == (lam("bs")(Sha2_256 $ Var("bs", sirByteString, AnE))))
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
