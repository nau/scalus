package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.{compile, fieldAsData}
import scalus.builtin.ByteString.*
import scalus.builtin.{Builtins, ByteString, Data, JVMPlatformSpecific}
import scalus.ledger.api.v1.*
import scalus.sir.SIRType
import scalus.prelude.List.{Cons, Nil}
import scalus.prelude.given
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.SIRType.{Boolean, Fun, TypeVar}
import scalus.sir.SirDSL.{*, given}
import scalus.sir.*
import scalus.uplc.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.eval.Result.Success
import scalus.uplc.eval.{PlutusVM, Result}

import scala.annotation.nowarn
import scala.collection.immutable
import scala.language.implicitConversions

object CompilerPluginToSIRSpecScope:

    case class ThreeInts(a: BigInt, b: BigInt, c: BigInt)

end CompilerPluginToSIRSpecScope

class CompilerPluginToSIRTest extends AnyFunSuite with ScalaCheckPropertyChecks:
    private given PlutusVM = PlutusVM.makePlutusV2VM()
    val deadbeef = Constant.ByteString(hex"deadbeef")

    val sirData = SIRType.Data
    val sirBool = SIRType.Boolean
    val sirInt = SIRType.Integer
    val sirString = SIRType.String
    val sirByteString = SIRType.ByteString
    val sirVoid = SIRType.Unit
    def sirList(tpe: SIRType) = SIRType.List(tpe)
    def sirBuiltinList(tpe: SIRType) = SIRType.BuiltinList(tpe)
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

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

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
              Binding(
                "a",
                SIRType.Boolean,
                Const(Constant.Bool(true), SIRType.Boolean, AnnotationsDecl.empty)
              )
            ),
            Var("a", SIRType.Boolean, AnnotationsDecl.empty),
            AnnotationsDecl.empty
          )
        )
    }

    test("compile def") {

        val compiled = compile {
            def b() = true

            def c(x: Boolean): Boolean = c(x)

            c(b())
        }

        val exprected = Let(
          Recursivity.NonRec,
          immutable.List(
            Binding(
              "b",
              SIRType.Fun(SIRType.Unit, SIRType.Boolean),
              LamAbs(
                Var("_", SIRType.Unit, AnE),
                Const(Constant.Bool(true), SIRType.Boolean, AnE),
                List.empty,
                AnE
              )
            )
          ),
          Let(
            Recursivity.Rec,
            immutable.List(
              Binding(
                "c",
                SIRType.Fun(SIRType.Boolean, SIRType.Boolean),
                LamAbs(
                  Var("x", SIRType.Boolean, AnE),
                  Apply(
                    Var("c", SIRType.Fun(Boolean, SIRType.Boolean), AnE),
                    Var("x", SIRType.Boolean, AnE),
                    SIRType.Boolean,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
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

        // println(s"comileDef: compiled: ${compiled.pretty.render(100)}")
        // println(s"comileDef: expected: ${exprected.pretty.render(100)}")

        assert(compiled ~=~ exprected)
    }

    test("compile lambda with args with type parameters") {
        // tail has a MethodType, check if it compiles
        val sir = compile {
            (tail: [A] => builtin.List[A] => builtin.List[A], ctx: builtin.List[Data]) =>
                tail[Data](ctx)
        }

        val compiledTp = sir.tp

        val a = TypeVar("A", Some(1), false)
        val builtinListA = SIRType.BuiltinList(a)
        val builtinListData = SIRType.BuiltinList(SIRType.Data)
        val tailType = SIRType.TypeLambda(List(a), Fun(builtinListA, builtinListA))

        val constructedExpr = LamAbs(
          Var("tail", tailType, AnE),
          LamAbs(
            Var("ctx", builtinListData, AnE),
            Apply(
              Var("tail", tailType, AnE),
              Var("ctx", builtinListData, AnE),
              builtinListData,
              AnE
            ),
            List.empty,
            AnE
          ),
          List.empty,
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
              Var("ctx", builtinListData, AnE),
              Apply(
                Var("tail", tailType, AnE),
                Var("ctx", builtinListData, AnE),
                builtinListData,
                AnE
              ),
              List.empty,
              AnE
            ),
            List.empty,
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
            List(
              Binding(
                "a",
                SIRType.Fun(sirBool, sirBool),
                LamAbs(Var("x", sirBool, AnE), Var("x", sirBool, AnE), List.empty, AnE)
              )
            ),
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
        class CustomException extends RuntimeException("CustomException")
        def foo(): Throwable = new CustomException
        inline def err(inline msg: String): Nothing = throw new RuntimeException(msg)
        // Compile message in the 1st Exception string literal argument to Error(message)
        assert(compile { throw new RuntimeException("foo") } ~=~ Error("foo", AnE))
        // Otherwise, compile Error(code.show)
        assert(
          compile { throw new RuntimeException(s"Not a literal: ${1 + 1}"): @nowarn } ~=~
              Error(
                "new RuntimeException(\n  _root_.scala.StringContext.apply([\"Not a literal: \",\"\" : String]).s([2 : Any])\n  ):RuntimeException @nowarn()",
                AnE
              )
        )
        // compile custom exceptions <:< Throwable as Error(exception.getSimpleName)
        assert(compile { throw new CustomException } ~=~ Error("CustomException", AnE))
        // compile throw code as Error(code.show)
        assert(compile { throw foo() } ~=~ Error("foo()", AnE))
        // handle inlines correctly
        assert(compile { err("test") } ~=~ Error("test", AnE))
    }

    test("compile ToData") {
        import scalus.builtin.Data.*
        val compiled = compile {
            BigInt(1).toData
        }
        val expected = {
            if summon[
                  Compiler.Options
                ].targetLoweringBackend == Compiler.TargetLoweringBackend.SirToUplcV3Lowering
            then {
                val a1Tp = SIRType.TypeVar("A", Some(1), false)
                val a2Tp = SIRType.TypeVar("A", Some(2), false)
                val a3Tp = SIRType.TypeVar("A", Some(3), false)
                Let(
                  NonRec,
                  immutable.List(
                    Binding(
                      "scalus.builtin.internal.UniversalDataConversion$.toData",
                      SIRType
                          .TypeLambda(immutable.List(a1Tp), SIRType.Fun(a1Tp, SIRType.TypeNothing)),
                      LamAbs(
                        SIR.Var("a", a1Tp, AnE),
                        SIR.Error(
                          "impossible to call this method at runtime, it is used only in the compiler plugin",
                          AnE
                        ),
                        immutable.List(a1Tp),
                        AnE
                      )
                    )
                  ),
                  Let(
                    NonRec,
                    immutable.List(
                      Binding("a$proxy1", sirInt, Const(Constant.Integer(1), sirInt, AnE))
                    ),
                    Apply(
                      ExternalVar(
                        "scalus.builtin.internal.UniversalDataConversion$",
                        "scalus.builtin.internal.UniversalDataConversion$.toData",
                        SIRType.Fun(sirInt, sirData),
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
            } else
                Let(
                  NonRec,
                  immutable.List(
                    Binding("a$proxy1", sirInt, Const(Constant.Integer(1), sirInt, AnE))
                  ),
                  Apply(
                    SIRBuiltins.iData,
                    Var("a$proxy1", sirInt, AnE),
                    sirData,
                    AnE
                  ),
                  AnE
                )
        }

        // println(s"compiled=${compiled.pretty.render(100)}")
        // println(s"exprected=${expected.pretty.render(100)}")
        //
        // SIRUnify.unifySIR(compiled, expected, SIRUnify.Env.empty) match
        //    case SIRUnify.UnificationSuccess(env, sir) =>
        //    case SIRUnify.UnificationFailure(path, left, right) =>
        //        println(s"Unification failure: $path, left=$left, right=$right")

        assert(compiled ~=~ expected)
        // val term = compiled.toUplc()
        // assert(VM.evaluateTerm(term) == Data.I(1))
    }

    test("compile chooseList builtins") {

        val x: SIR = List(1, 2, 3)
        println(s"!!!: x = ${x.pretty.render(100)}")
        println(s"!!!x: x= $x ")

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
          List(Binding("a", sirString, Const(Constant.String("foo"), sirString, AnE))),
          Apply(
            Apply(
              SIRBuiltins.mkCons,
              Const(Constant.String("bar"), sirString, AnE),
              Fun(SIRType.BuiltinList(sirString), SIRType.BuiltinList(sirString)),
              AnE
            ),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Var("a", sirString, AnE),
                Fun(SIRType.BuiltinList(sirString), SIRType.BuiltinList(sirString)),
                AnE
              ),
              Const(Constant.List(DefaultUni.String, List()), SIRType.BuiltinList(sirString), AnE),
              SIRType.BuiltinList(sirString),
              AnE
            ),
            SIRType.BuiltinList(sirString),
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
                Var("l", sirBuiltinList(sirInt), AnE),
                Apply(SIRBuiltins.headList, Var("l", sirBuiltinList(sirInt), AnE), sirInt, AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile tail function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.tail }
              ~=~ LamAbs(
                Var("l", sirBuiltinList(sirInt), AnE),
                Apply(
                  SIRBuiltins.tailList,
                  Var("l", sirBuiltinList(sirInt), AnE),
                  sirBuiltinList(sirInt),
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile isEmpty function") {
        assert(
          compile { (l: builtin.List[BigInt]) => l.isEmpty }
              ~=~ LamAbs(
                Var("l", sirBuiltinList(sirInt), AnE),
                Apply(SIRBuiltins.nullList, Var("l", sirBuiltinList(sirInt), AnE), sirBool, AnE),
                List.empty,
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
                    sirBuiltinList(sirData),
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
                    SIRType.BuiltinList(SIRType.Pair(sirData, sirData)),
                    AnE
                  ))
        )
    }

    test("compile constrData builtins") {
        val nilData =
            Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.BuiltinList(sirData), AnE)
        assert(
          compile(
            Builtins.constrData(
              1,
              builtin.List(Builtins.iData(2))
            )
          ) ~=~ Apply(
            Apply(SIRBuiltins.constrData, sirConst(1), Fun(sirBuiltinList(sirData), sirData), AnE),
            Apply(
              Apply(
                SIRBuiltins.mkCons,
                Apply(SIRBuiltins.iData, sirConst(2), sirData, AnE),
                Fun(sirBuiltinList(sirData), sirBuiltinList(sirData)),
                AnE
              ),
              nilData,
              sirBuiltinList(sirData),
              AnE
            ),
            sirData,
            AnE
          )
        )
    }

    test("compile listData builtins") {
        val nilData =
            Const(Constant.List(DefaultUni.Data, immutable.Nil), SIRType.BuiltinList(sirData), AnE)
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
                        Fun(sirBuiltinList(sirData), sirBuiltinList(sirData)),
                        AnE
                      ),
                      nilData,
                      sirBuiltinList(sirData),
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

    test("compile mapData builtins") {

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
                Fun(
                  sirBuiltinList(sirPair(sirData, sirData)),
                  sirBuiltinList(sirPair(sirData, sirData))
                ),
                AnE
              ),
              Const(
                Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), immutable.Nil),
                SIRType.BuiltinList(sirPair(sirData, sirData)),
                AnE
              ),
              sirBuiltinList(sirPair(sirData, sirData)),
              AnE
            ),
            sirData,
            AnE
          )
        )
    }

    test("compile unConstrData function") {
        assert(
          compile { (d: Data) => Builtins.unConstrData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(
                  SIRBuiltins.unConstrData,
                  Var("d", sirData, AnE),
                  sirPair(sirInt, sirBuiltinList(sirData)),
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile unListData function") {
        assert(
          compile { (d: Data) => Builtins.unListData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unListData, Var("d", sirData, AnE), sirBuiltinList(sirData), AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile unMapData function") {
        assert(
          compile { (d: Data) => Builtins.unMapData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(
                  SIRBuiltins.unMapData,
                  Var("d", sirData, AnE),
                  sirBuiltinList(sirPair(sirData, sirData)),
                  AnE
                ),
                List.empty,
                AnE
              )
        )
    }

    test("compile unBData function") {
        assert(
          compile { (d: Data) => Builtins.unBData(d) }
              ~=~ LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unBData, Var("d", sirData, AnE), sirByteString, AnE),
                List.empty,
                AnE
              )
        )
    }

    test("compile unIData function") {
        assert(
          compile { (d: Data) => Builtins.unIData(d) } ~=~
              LamAbs(
                Var("d", sirData, AnE),
                Apply(SIRBuiltins.unIData, Var("d", sirData, AnE), sirInt, AnE),
                List.empty,
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
                List.empty,
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
                  List.empty,
                  AnE
                ),
                List.empty,
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
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
                List.empty,
                AnE
              )
        )
        assert(
          compile(Builtins.bls12_381_G1_scalarMul) ~=~ LamAbs(
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
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )

        assert(
          compile(Builtins.bls12_381_G1_equal) ~=~ LamAbs(
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
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )

        assert(
          compile(Builtins.bls12_381_G1_hashToGroup) ~=~ LamAbs(
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
              List.empty,
              AnE
            ),
            List.empty,
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
            List.empty,
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
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_neg) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_neg, pVar, SIRType.BLS12_381_G2_Element, AnE),
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_compress) ~=~ LamAbs(
            pVar,
            Apply(SIRBuiltins.bls12_381_G2_compress, pVar, SIRType.ByteString, AnE),
            List.empty,
            AnE
          )
        )
        assert(
          compile(Builtins.bls12_381_G2_uncompress) ~=~ LamAbs(
            bsVar,
            Apply(SIRBuiltins.bls12_381_G2_uncompress, bsVar, SIRType.BLS12_381_G2_Element, AnE),
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
            List.empty,
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
            List.empty,
            AnE
          )
        )
    }

    test("compile BigInt ops") {
        assert(compile(-BigInt(-1)) ~=~ (SubtractInteger $ 0 $ -1))
        assert(compile(BigInt(1) + 2) ~=~ (AddInteger $ 1 $ 2))
        assert(compile(BigInt(1) - 2) ~=~ (SubtractInteger $ 1 $ 2))
        assert(compile(BigInt(1) * 2) ~=~ (MultiplyInteger $ 1 $ 2))
        assert(compile(BigInt(1) / 2) ~=~ (DivideInteger $ 1 $ 2))
        assert(compile(BigInt(1) % 2) ~=~ (RemainderInteger $ 1 $ 2))
        assert(compile(BigInt(1) < 2) ~=~ (LessThanInteger $ 1 $ 2))
        assert(compile(BigInt(1) <= 2) ~=~ (LessThanEqualsInteger $ 1 $ 2))
        assert(compile(BigInt(1) > 2) ~=~ (LessThanInteger $ 2 $ 1))
        assert(compile(BigInt(1) >= 2) ~=~ (LessThanEqualsInteger $ 2 $ 1))
        assert(compile(BigInt(1) == BigInt(2)) ~=~ (EqualsInteger $ 1 $ 2))
        assert(compile(BigInt(1) != BigInt(2)) ~=~ Not(EqualsInteger $ 1 $ 2, AnE))
    }

    test("compile Integer builtins") {
        assert(compile(Builtins.addInteger(1, 2)) ~=~ (AddInteger $ 1 $ 2))
        assert(compile(Builtins.subtractInteger(1, 2)) ~=~ (SubtractInteger $ 1 $ 2))
        assert(compile(Builtins.multiplyInteger(1, 2)) ~=~ (MultiplyInteger $ 1 $ 2))
        assert(compile(Builtins.divideInteger(1, 2)) ~=~ (DivideInteger $ 1 $ 2))
        assert(compile(Builtins.modInteger(1, 2)) ~=~ (ModInteger $ 1 $ 2))
        assert(compile(Builtins.quotientInteger(1, 2)) ~=~ (QuotientInteger $ 1 $ 2))
        assert(compile(Builtins.remainderInteger(1, 2)) ~=~ (RemainderInteger $ 1 $ 2))
        assert(compile(Builtins.lessThanInteger(1, 2)) ~=~ (LessThanInteger $ 1 $ 2))
        assert(compile(Builtins.lessThanEqualsInteger(1, 2)) ~=~ (LessThanEqualsInteger $ 1 $ 2))
        assert(compile(Builtins.equalsInteger(1, 2)) ~=~ (EqualsInteger $ 1 $ 2))
    }

    test("compile ByteStrings builtins") {
        assert(
          compile(
            Builtins.appendByteString(hex"dead", hex"beef")
          ) ~=~ (AppendByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.sliceByteString(1, 2, hex"dead")
          ) ~=~ (SliceByteString $ 1 $ 2 $ hex"dead")
        )

        assert(
          compile(
            Builtins.lengthOfByteString(hex"dead")
          ) ~=~ (LengthOfByteString $ hex"dead")
        )

        assert(
          compile(
            Builtins.indexByteString(hex"dead", 1)
          ) ~=~ (IndexByteString $ hex"dead" $ 1)
        )

        assert(
          compile(
            Builtins.equalsByteString(hex"dead", hex"beef")
          ) ~=~ (EqualsByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.lessThanByteString(hex"dead", hex"beef")
          ) ~=~ (LessThanByteString $ hex"dead" $ hex"beef")
        )

        assert(
          compile(
            Builtins.lessThanEqualsByteString(hex"dead", hex"beef")
          ) ~=~ (LessThanEqualsByteString $ hex"dead" $ hex"beef")
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
        assert(compile(Builtins.sha2_256(hex"dead")) ~=~ (Sha2_256 $ hex"dead"))
        assert(compile(Builtins.sha3_256(hex"dead")) ~=~ (Sha3_256 $ hex"dead"))
        assert(compile(Builtins.blake2b_256(hex"dead")) ~=~ (Blake2b_256 $ hex"dead"))
        assert(
          compile(
            Builtins.verifyEd25519Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) ~=~ (VerifyEd25519Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
        assert(
          compile(
            Builtins.verifyEcdsaSecp256k1Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) ~=~ (VerifyEcdsaSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
        assert(
          compile(
            Builtins.verifySchnorrSecp256k1Signature(
              hex"dead",
              hex"beef",
              hex"cafe"
            )
          ) ~=~ (VerifySchnorrSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
        )
    }

    test("compile String builtins") {
        assert(compile(Builtins.appendString("dead", "beef")) ~=~ (AppendString $ "dead" $ "beef"))
        assert(compile(Builtins.equalsString("dead", "beef")) ~=~ (EqualsString $ "dead" $ "beef"))
        assert(compile(Builtins.encodeUtf8("dead")) ~=~ (EncodeUtf8 $ "dead"))
        assert(compile(Builtins.decodeUtf8(hex"dead")) ~=~ (DecodeUtf8 $ hex"dead"))
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
              List.empty,
              AnE
            ),
            List.empty,
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
                List.empty,
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
                List.empty,
                AnE
              )
        )

    }

    test("compile custom Builtins") {
        val platform = new JVMPlatformSpecific {
            override def sha2_256(bs: ByteString): ByteString = hex"deadbeef"
        }
        object CustomBuiltins extends Builtins(using platform)

        val sir = compile(CustomBuiltins.sha2_256(hex"12"))
        // check that SIRCompiler compiles the custom builtin
        assert(
          sir ~=~ Apply(
            SIRBuiltins.sha2_256,
            Const(Constant.ByteString(hex"12"), sirByteString, AnE),
            sirByteString,
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
                List(Binding("a", sirBool, Or(sirConst(true), Error("M", AnE), AnE))),
                Or(
                  And(Not(Var("a", sirBool, AnE), AnE), sirConst(false), AnE),
                  sirConst(true),
                  AnE
                ),
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
                SIRType.Fun(sirBool, sirBool),
                LamAbs(
                  aVar,
                  SIR.IfThenElse(
                    aVar,
                    sirConst(false),
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool, AnE),
                    sirBool,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              aVar,
              Apply(Var("check", Fun(sirBool, sirBool), AnE), aVar, sirBool, AnE),
              List.empty,
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
                SIRType.Fun(sirBool, sirBool),
                LamAbs(
                  aVar,
                  SIR.IfThenElse(
                    aVar,
                    SIR.IfThenElse(sirConst(false), sirConst(false), sirConst(true), sirBool, AnE),
                    sirConst(false),
                    sirBool,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              aVar,
              Apply(Var("check", Fun(sirBool, sirBool), AnE), aVar, sirBool, AnE),
              List.empty,
              AnE
            ),
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
              SIRType.Fun(sirByteString, Fun(sirByteString, sirBool)),
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
                  List.empty,
                  AnE
                ),
                List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
                SIRType.Fun(sirByteString, Fun(sirByteString, sirBool)),
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
                      ),
                      AnE
                    ),
                    List.empty,
                    AnE
                  ),
                  List.empty,
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
                List.empty,
                AnE
              ),
              List.empty,
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
                SIRType.Fun(sirString, Fun(sirString, sirBool)),
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
                    List.empty,
                    AnE
                  ),
                  List.empty,
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
                List.empty,
                AnE
              ),
              List.empty,
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
              SIRType.Fun(sirString, Fun(sirString, sirBool)),
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
                    ),
                    AnE
                  ),
                  List.empty,
                  AnE
                ),
                List.empty,
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
              List.empty,
              AnE
            ),
            List.empty,
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
            NonRec,
            List(
              Binding(
                "check",
                SIRType.Fun(sirData, Fun(sirData, sirBool)),
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
                    List.empty,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              a,
              LamAbs(
                b,
                Apply(Apply(check, a, Fun(sirData, sirBool), AnE), b, sirBool, AnE),
                List.empty,
                AnE
              ),
              List.empty,
              AnE
            ),
            AnE
          )
        )
        assert(
          ne ~=~ Let(
            NonRec,
            List(
              Binding(
                "check",
                SIRType.Fun(sirData, Fun(sirData, sirBool)),
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
                      ),
                      AnE
                    ),
                    List.empty,
                    AnE
                  ),
                  List.empty,
                  AnE
                )
              )
            ),
            LamAbs(
              a,
              LamAbs(
                b,
                Apply(Apply(check, a, Fun(sirData, sirBool), AnE), b, sirBool, AnE),
                List.empty,
                AnE
              ),
              List.empty,
              AnE
            ),
            AnE
          )
        )
        val eqterm = eq.toUplc()
        val neterm = ne.toUplc()
        import scalus.builtin.Data.toData
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
        import scalus.prelude.*
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
                pubKeyHashDataDecl.constrType("scalus.ledger.api.v1.PubKeyHash"),
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
          compiled ~=~
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

    test("compile fieldAsData macro") {
        import scalus.ledger.api.v1.*

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
        val appliedScript = term.plutusV1 $ scriptContext.toData
        assert(appliedScript.evaluate == scalus.uplc.Term.Const(asConstant(hex"deadbeef")))
        val flatBytesLength = appliedScript.flatEncoded.length
        summon[Compiler.Options].targetLoweringBackend match
            case Compiler.TargetLoweringBackend.SirToUplcV3Lowering =>
                assert(flatBytesLength == 168)
            case _ =>
                assert(flatBytesLength == 348)
    }

    test("@Ignore annotation") {
        @Ignore
        def foo() = 1

        assert(compile {
            @Ignore val a = true

            @Ignore def foo() = true
        } ~=~ Const(Constant.Unit, SIRType.Unit, AnE))
    }

    test("compile pattern in val with one argument") {
        import scalus.prelude.Option
        import scalus.builtin.FromData
        import scalus.builtin.ToData
        val compiled = compile { (x: Data) =>
            val Option.Some(v0) = summon[FromData[Option[BigInt]]](x): @unchecked
            // val Option.Some(v) = x
            v0

        }
        val uplcFun = compiled.toUplc(generateErrorTraces = true)

        val dataSome1 = summon[ToData[Option[BigInt]]].apply(Option.Some(BigInt(1)))
        val uplc1 = uplcFun $ Term.Const(Constant.Data(dataSome1))
        val script1 = uplc1.plutusV3

        script1.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
            case Result.Failure(exception, _, _, _) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

        val dataNone = summon[ToData[Option[BigInt]]].apply(Option.None)
        val uplc2 = uplcFun $ Term.Const(Constant.Data(dataNone))
        val script2 = uplc2.plutusV3

        script2.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                fail("should not be successful")
            case Result.Failure(exception, _, _, logs) =>
                assert(logs.exists(_.contains("Unexpected case")))
            // assert(logs == List("Pattern match failure: expected Some but got None"))
    }

    test("compile pattern with val with two arguments") {
        import scalus.prelude.List
        import scalus.builtin.FromData
        import scalus.builtin.ToData

        val compiled = compile { (x: Data) =>
            val List.Cons(head, tail) = summon[FromData[List[BigInt]]](x): @unchecked
            // val Option.Some(v) = x
            head + tail.length
        }

        val uplcFun = compiled.toUplc(generateErrorTraces = true)

        val dataCons1 = summon[ToData[List[BigInt]]].apply(List.Cons(BigInt(1), List.Nil))
        val uplc1 = uplcFun $ Term.Const(Constant.Data(dataCons1))
        val script1 = uplc1.plutusV3
        script1.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
            case Result.Failure(exception, _, _, logs) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

        val dataCons2 =
            summon[ToData[List[BigInt]]].apply(List.Cons(BigInt(1), List.Cons(BigInt(2), List.Nil)))
        val uplc2 = uplcFun $ Term.Const(Constant.Data(dataCons2))
        val script2 = uplc2.plutusV3
        script2.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                assert(evaled == scalus.uplc.Term.Const(Constant.Integer(2)))
            case Result.Failure(exception, _, _, logs) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

        val dataNil = summon[ToData[List[BigInt]]].apply(List.Nil)
        val uplc3 = uplcFun $ Term.Const(Constant.Data(dataNil))
        val script3 = uplc3.plutusV3
        script3.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
                fail("should not be successful")
            case Result.Failure(exception, _, _, logs) =>
                assert(logs.exists(_.contains("Unexpected case")))

    }

    test("Compile pattern in val with three arguments") {
        import scalus.prelude.List
        import scalus.prelude.Option
        import scalus.builtin.Data.FromData
        import scalus.builtin.Data.ToData
        import scalus.ledger.api.v3.*

        val compiled = compile { (x: Data) =>
            val ScriptContext(txInfo, redeemer, scriptInfo) = summon[FromData[ScriptContext]](x)
            // val Option.Some(v) = x
            txInfo
        }

        val uplcFun = compiled.toUplc(generateErrorTraces = true)

        val txInfo = TxInfo(
          inputs = Nil,
          id = TxId(hex"bb")
        )
        val redeemer = Data.unit
        val scriptInfo = ScriptInfo.SpendingScript(
          TxOutRef(TxId(hex"deadbeef"), 0),
          Option.None
        )
        val dataScriptInfo =
            summon[ToData[ScriptContext]].apply(ScriptContext(txInfo, redeemer, scriptInfo))
        val uplc1 = uplcFun $ Term.Const(Constant.Data(dataScriptInfo))
        val script1 = uplc1.plutusV3
        script1.evaluateDebug match
            case Result.Success(evaled, _, _, logs) =>
            // val txInfo1Data = summon[ToData[TxInfo]].apply()
            case Result.Failure(exception, _, _, logs) =>
                println("failure: exception=" + exception.getMessage)
                fail(exception)

    }

    test("compile scala function with zero arguments") {
        val compiled = compile {
            def fooZeroArgs = BigInt(1)
            val z = fooZeroArgs
            z
        }

        val evaluated = compiled.toUplc().evaluate

        assert(evaluated == scalus.uplc.Term.Const(Constant.Integer(1)))

    }

    test("compile scala functions with zero arguments and type parameter") {
        val compiled = compile {
            def fooZeroArgsTp[T]: scalus.prelude.List[T] = scalus.prelude.List.empty[T]
            val z = fooZeroArgsTp[BigInt]
            z.isEmpty
        }

        // println(s"sir=${compiled.pretty.render(100)}")

        val uplc = compiled.toUplc(generateErrorTraces = true)
        // println(s"uplc=${uplc.pretty.render(100)}")

        val evaluated = compiled.toUplc().evaluate

        assert(evaluated == scalus.uplc.Term.Const(Constant.Bool(true)))

        // println(s"evaluated=${evaluated}")
    }

    test("compile scala methd with zero arguments") {
        val compiled = compile {
            import scalus.prelude.*
            val m: SortedMap[BigInt, BigInt] = SortedMap.empty[BigInt, BigInt]
            m.get(BigInt(1)) match {
                case Option.None    => BigInt(0)
                case Option.Some(v) => v
            }
        }

        // println(s"sir=${compiled.pretty.render(100)}")

        def retrieveLastSIRComponent(sir: SIR): SIR =
            sir match
                case SIR.Decl(data, term) => retrieveLastSIRComponent(term)
                case _                    => sir

        def findLetForVar(sir: SIR, name: String): Option[SIR.Let] =
            sir match
                case SIR.Let(_, bindings, body, _) =>
                    bindings.find(_.name == name) match
                        case Some(binding) => Some(SIR.Let(NonRec, List(binding), body, AnE))
                        case None          => findLetForVar(body, name)
                case _ => None

        val mLet = findLetForVar(retrieveLastSIRComponent(compiled), "m").get

        val mBindingCompiled = mLet.bindings.head

        val mBindingExpected =
            Binding(
              "m",
              SIRType.FreeUnificator,
              Apply(
                SIR.ExternalVar(
                  "scalus.prelude.SortedMap$",
                  "scalus.prelude.SortedMap$.empty",
                  SIRType.FreeUnificator,
                  AnE
                ),
                sirConstUnit,
                SIRType.FreeUnificator,
                AnE
              )
            )

        assert(
          mBindingCompiled.value ~=~ mBindingExpected.value
        )

        val uplc = compiled.toUplcOptimized(generateErrorTraces = true)
        val evaluated = uplc.evaluate
        assert(evaluated == scalus.uplc.Term.Const(Constant.Integer(0)))

    }

    test("compile scala methd with zero arguments in non-var position") {

        val compiled = compile {
            import scalus.prelude.*
            SortedMap.empty[BigInt, BigInt].get(BigInt(1)) match {
                case Option.None    => BigInt(0)
                case Option.Some(v) => v
            }
        }

        val uplc = compiled.toUplcOptimized(generateErrorTraces = true)
        val evaluated = uplc.evaluate
        assert(evaluated == scalus.uplc.Term.Const(Constant.Integer(0)))

    }
