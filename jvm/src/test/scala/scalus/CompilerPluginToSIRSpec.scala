package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.Data
import scalus.builtin.given
import scalus.ledger.api.v1.*
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.prelude.Prelude.given
import scalus.sir.Binding
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Recursivity
import scalus.sir.Recursivity.*
import scalus.sir.SIR
import scalus.sir.SIR.*
import scalus.sir.SirDSL.{*, given}
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.*
import scalus.uplc.eval.VM

import scala.collection.immutable
import scala.language.implicitConversions

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
    val deadbeef = Constant.ByteString(hex"deadbeef")

    test("compile literals") {
        assert(compile(false) == Const(Constant.Bool(false)))
        assert(compile(true) == Const(Constant.Bool(true)))
        assert(compile(()) == Const(Constant.Unit))
        assert(compile("foo") == Const(Constant.String("foo")))
        assert(
          compile(BigInt("15511210043330985984000000")) == Const(
            Constant.Integer(BigInt("15511210043330985984000000"))
          )
        )
        assert(compile(12: BigInt) == Const(Constant.Integer(BigInt("12"))))
        assert(compile(scala.math.BigInt.int2bigInt(12)) == Const(Constant.Integer(BigInt("12"))))

        // ByteStrings
        assert(
          compile(builtin.ByteString.empty) == Const(
            Constant.ByteString(builtin.ByteString.empty)
          )
        )

        assert(compile(builtin.ByteString.fromHex("deadbeef")) == Const(deadbeef))
        assert(compile(hex"deadbeef") == Const(deadbeef))
        assert(
          compile(builtin.ByteString.fromString("deadbeef")) == Const(
            Constant.ByteString(builtin.ByteString.fromString("deadbeef"))
          )
        )
    }

    test("compile if-then-else") {
        assert(
          compile {
              if Builtins.equalsInteger(1, 2) then () else ()
          } == SIR.IfThenElse(
            Apply(
              Apply(Builtin(EqualsInteger), Const(Constant.Integer(1))),
              Const(Constant.Integer(2))
            ),
            Const(Constant.Unit),
            Const(Constant.Unit)
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
            immutable.List(Binding("a", Const(Constant.Bool(true)))),
            Var("a")
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
            immutable.List(Binding("b", LamAbs("_", Const(Constant.Bool(true))))),
            Let(
              Recursivity.Rec,
              immutable.List(Binding("c", LamAbs("x", Var("x")))),
              Apply(Var("c"), Apply(Var("b"), Const(Constant.Unit)))
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
        assert(
          sir == LamAbs("tail", LamAbs("ctx", Apply(Var("tail"), Var("ctx"))))
        )
    }

    test("compile inline def") {
        assert(
          compile {
              inline def b = true

              b
          } == Const(Constant.Bool(true))
        )
    }

    test("compile lambda") {
        assert(
          compile {
              val a = (x: Boolean) => x
              a(true)
          } == Let(
            NonRec,
            List(Binding("a", LamAbs("x", Var("x")))),
            Apply(Var("a"), Const(Constant.Bool(true)))
          )
        )
    }

    test("compile throw") {
        assert(compile {
            throw new RuntimeException("foo")
        } == Error("foo"))
    }

    test("compile ToData") {
        import scalus.builtin.Data.*
        import scalus.builtin.ToDataInstances.given
        val compiled = compile {
            BigInt(1).toData
        }
        assert(
          compiled == Let(
            Rec,
            immutable.List(
              Binding(
                "scalus.builtin.ToDataInstances$.given_ToData_BigInt",
                LamAbs("a", Apply(Builtin(IData), Var("a")))
              )
            ),
            Let(
              NonRec,
              immutable.List(Binding("a$proxy1", Const(Constant.Integer(1)))),
              Apply(Var("scalus.builtin.ToDataInstances$.given_ToData_BigInt"), Var("a$proxy1"))
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
          ) == (DefaultFun.ChooseList $ List(1, 2, 3) $ true $ false)
        )
    }

    test("compile mkCons builtins") {
        assert(
          compile(
            Builtins.mkCons(BigInt(4), builtin.List[BigInt](1, 2, 3))
          ) == (DefaultFun.MkCons $ 4 $ List(1, 2, 3))
        )
    }

    test("compile headList builtins") {
        assert(
          compile(
            Builtins.headList(builtin.List[BigInt](1, 2, 3))
          ) == (DefaultFun.HeadList $ List(1, 2, 3))
        )
    }

    test("compile tailList builtins") {
        assert(
          compile(
            Builtins.tailList(builtin.List[BigInt](1, 2, 3))
          ) == (DefaultFun.TailList $ List(1, 2, 3))
        )
    }

    test("compile nullList builtins") {
        assert(
          compile(
            Builtins.nullList(builtin.List[BigInt](1, 2, 3))
          ) == (DefaultFun.NullList $ List(1, 2, 3))
        )
    }

    test("compile empty List") {
        assert(
          compile {
              builtin.List.empty[BigInt]
          } == Const(Constant.List(DefaultUni.Integer, List()))
        )
    }

    test("compile List literal") {
        assert(
          compile {
              builtin.List[BigInt](1, 2, 3)
          } == Const(
            Constant.List(
              DefaultUni.Integer,
              List(Constant.Integer(1), Constant.Integer(2), Constant.Integer(3))
            )
          )
        )
    }

    test("compile MkCons builtin") {
        assert(
          compile {
              val a = "foo"
              "bar" :: builtin.List(a)
          } == Let(
            NonRec,
            List(Binding("a", Const(Constant.String("foo")))),
            Apply(
              Apply(Builtin(MkCons), Const(Constant.String("bar"))),
              Apply(
                Apply(Builtin(MkCons), Var("a")),
                Const(Constant.List(DefaultUni.String, List()))
              )
            )
          )
        )
    }

    test("compile head function") {
        assert(
          compile {
              def head(l: builtin.List[BigInt]) = l.head
          } == Let(
            Rec,
            List(
              Binding("head", LamAbs("l", Apply(Builtin(HeadList), Var("l"))))
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile tail function") {
        assert(
          compile {
              def tail(l: builtin.List[BigInt]) = l.tail
          } == Let(
            Rec,
            List(
              Binding("tail", LamAbs("l", Apply(Builtin(TailList), Var("l"))))
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile isEmpty function") {
        assert(
          compile {
              def isEmpty(l: builtin.List[BigInt]) = l.isEmpty
          } == Let(
            Rec,
            List(
              Binding("isEmpty", LamAbs("l", Apply(Builtin(NullList), Var("l"))))
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile mkNilData") {
        assert(compile(Builtins.mkNilData()) == (Apply(Builtin(MkNilData), Const(Constant.Unit))))
    }

    test("compile mkNilPairData") {
        assert(
          compile(Builtins.mkNilPairData()) == (Apply(Builtin(MkNilPairData), Const(Constant.Unit)))
        )
    }

    test("compile mkConstr builtins") {
        val nilData = Const(Constant.List(DefaultUni.Data, immutable.Nil))
        assert(
          compile(
            Builtins.constrData(
              1,
              builtin.List(Builtins.iData(2))
            )
          ) == Apply(
            Apply(Builtin(ConstrData), Const(Constant.Integer(1))),
            Apply(
              Apply(Builtin(MkCons), Apply(Builtin(IData), Const(Constant.Integer(2)))),
              nilData
            )
          )
        )
    }

    test("compile mkList builtins") {
        val nilData = Const(Constant.List(DefaultUni.Data, immutable.Nil))
        assert(
          compile(
            Builtins.listData(builtin.List(Builtins.iData(1)))
          ) == Apply(
            Builtin(ListData),
            Apply(
              Apply(Builtin(MkCons), Apply(Builtin(IData), Const(Constant.Integer(1)))),
              nilData
            )
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
          ) == Apply(
            Builtin(MapData),
            Apply(
              Apply(
                Builtin(MkCons),
                Apply(
                  Apply(Builtin(MkPairData), Apply(Builtin(BData), Const(deadbeef))),
                  Apply(Builtin(IData), Const(Constant.Integer(1)))
                )
              ),
              Const(Constant.List(DefaultUni.Pair(DefaultUni.Data, DefaultUni.Data), immutable.Nil))
            )
          )
        )
    }

    test("compile unsafeDataAsConstr function") {
        assert(
          compile {
              def unb(d: Data) = Builtins.unConstrData(d)
          } == Let(
            Rec,
            List(
              Binding(
                "unb",
                LamAbs("d", Apply(Builtin(DefaultFun.UnConstrData), Var("d")))
              )
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile unsafeDataAsList function") {
        assert(
          compile {
              def unb(d: Data) = Builtins.unListData(d)
          } == Let(
            Rec,
            List(
              Binding(
                "unb",
                LamAbs("d", Apply(Builtin(DefaultFun.UnListData), Var("d")))
              )
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile unsafeDataAsMap function") {
        assert(
          compile {
              def unb(d: Data) = Builtins.unMapData(d)
          } == Let(
            Rec,
            List(
              Binding(
                "unb",
                LamAbs("d", Apply(Builtin(DefaultFun.UnMapData), Var("d")))
              )
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile unsafeDataAsB function") {
        assert(
          compile {
              def unb(d: Data) = Builtins.unBData(d)
          } == Let(
            Rec,
            List(
              Binding(
                "unb",
                LamAbs("d", Apply(Builtin(DefaultFun.UnBData), Var("d")))
              )
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile unsafeDataAsI function") {
        assert(
          compile {
              def unb(d: Data) = Builtins.unIData(d)
          } == Let(
            Rec,
            List(
              Binding(
                "unb",
                LamAbs("d", Apply(Builtin(DefaultFun.UnIData), Var("d")))
              )
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile chooseData function") {
        assert(
          compile {
              def cd(d: Data) = Builtins.chooseData[BigInt](d, 1, 2, 3, 4, 5)
          } == Let(
            Rec,
            List(
              Binding(
                "cd",
                LamAbs(
                  "d",
                  ChooseData $ Var("d") $ 1 $ 2 $ 3 $ 4 $ 5
                )
              )
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile equalsData function") {
        assert(
          compile {
              def ed(d1: Data, d2: Data) = Builtins.equalsData(d1, d2)
          } == Let(
            Rec,
            List(
              Binding("ed", LamAbs("d1", LamAbs("d2", EqualsData $ Var("d1") $ Var("d2"))))
            ),
            Const(Constant.Unit)
          )
        )
    }

    test("compile serialiseData builtins") {
        assert(
          compile {
              Builtins.serialiseData
          } == Builtin(DefaultFun.SerialiseData)
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
        assert(compile(Builtins.mkPairData) == (Builtin(MkPairData)))
        assert(
          compile {
              def swap(p: builtin.Pair[Data, Data]) =
                  builtin.Pair(Builtins.sndPair(p), Builtins.fstPair(p))
          } == (
            Let(
              Rec,
              List(
                Binding(
                  "swap",
                  LamAbs("p", MkPairData $ (SndPair $ Var("p")) $ (FstPair $ Var("p")))
                )
              ),
              Const(Constant.Unit)
            )
          )
        )
        assert(compile {
            builtin.Pair(BigInt(1), hex"deadbeef")
        } == (Const(Constant.Pair(Constant.Integer(1), deadbeef))))
        assert(
          compile {
              def swap(p: builtin.Pair[Data, Data]) = builtin.Pair(p.snd, p.fst)
          } == (
            Let(
              Rec,
              List(
                Binding(
                  "swap",
                  LamAbs(
                    "p",
                    Apply(
                      Apply(Builtin(MkPairData), Apply(Builtin(SndPair), Var("p"))),
                      Apply(Builtin(FstPair), Var("p"))
                    )
                  )
                )
              ),
              Const(Constant.Unit)
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
          compiled ==
              Let(
                NonRec,
                List(Binding("a", Or(Const(Bool(true)), Error("M")))),
                Or(And(Not(Var("a")), Const(Bool(false))), Const(Bool(true)))
              )
        )
        // println(compiled.pretty.render(80))
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
        // println(evaled.pretty.render(80))
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
    }

    test("compile Boolean equality") {
        import Constant.Bool
        val eq = compile { def check(a: Boolean) = a == false; check }
        val ne = compile { def check(a: Boolean) = a != false; check }
        assert(
          eq == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  SIR.IfThenElse(
                    Var("a"),
                    Const(Bool(false)),
                    SIR.IfThenElse(Const(Bool(false)), Const(Bool(false)), Const(Bool(true)))
                  )
                )
              )
            ),
            LamAbs("a", Apply(Var("check"), Var("a")))
          )
        )
        assert(
          ne == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  SIR.IfThenElse(
                    Var("a"),
                    SIR.IfThenElse(Const(Bool(false)), Const(Bool(false)), Const(Bool(true))),
                    Const(Bool(false))
                  )
                )
              )
            ),
            LamAbs("a", Apply(Var("check"), Var("a")))
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
          eq == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  LamAbs("b", Apply(Apply(Builtin(EqualsByteString), Var("a")), Var("b")))
                )
              )
            ),
            LamAbs("a", LamAbs("b", Apply(Apply(Var("check"), Var("a")), Var("b"))))
          )
        )
        assert(
          ne == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  LamAbs("b", Not(Apply(Apply(Builtin(EqualsByteString), Var("a")), Var("b"))))
                )
              )
            ),
            LamAbs("a", LamAbs("b", Apply(Apply(Var("check"), Var("a")), Var("b"))))
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
        assert(
          eq == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  LamAbs("b", Apply(Apply(Builtin(EqualsString), Var("a")), Var("b")))
                )
              )
            ),
            LamAbs("a", LamAbs("b", Apply(Apply(Var("check"), Var("a")), Var("b"))))
          )
        )
        assert(
          ne == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  LamAbs("b", Not(Apply(Apply(Builtin(EqualsString), Var("a")), Var("b"))))
                )
              )
            ),
            LamAbs("a", LamAbs("b", Apply(Apply(Var("check"), Var("a")), Var("b"))))
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
        assert(
          eq == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  LamAbs("b", Apply(Apply(Builtin(EqualsData), Var("a")), Var("b")))
                )
              )
            ),
            LamAbs("a", LamAbs("b", Apply(Apply(Var("check"), Var("a")), Var("b"))))
          )
        )
        assert(
          ne == Let(
            Rec,
            List(
              Binding(
                "check",
                LamAbs(
                  "a",
                  LamAbs("b", Not(Apply(Apply(Builtin(EqualsData), Var("a")), Var("b"))))
                )
              )
            ),
            LamAbs("a", LamAbs("b", Apply(Apply(Var("check"), Var("a")), Var("b"))))
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
        // println(compiled.pretty.render(80))
        val term = compiled.toUplc()
        // println(term.pretty.render(80))
        val evaled = VM.evaluateTerm(term)
        assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
    }

    test("compile external definitions") {
        def foo(i: BigInt) = i

        assert(
          compile {
              foo(5)
          } ==
              Let(
                Rec,
                List(
                  Binding("scalus.CompilerPluginToSIRSpec._$_$foo", LamAbs("i", Var("i")))
                ),
                Apply(
                  Var("scalus.CompilerPluginToSIRSpec._$_$foo"),
                  Const(Constant.Integer(5))
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
        assert(
          compiled ==
              Decl(
                DataDecl("PubKeyHash", List(ConstrDecl("PubKeyHash", List("hash")))),
                Let(
                  NonRec,
                  List(
                    Binding(
                      "pkh",
                      Constr(
                        "PubKeyHash",
                        DataDecl("PubKeyHash", List(ConstrDecl("PubKeyHash", List("hash")))),
                        List(Const(uplc.Constant.ByteString(hex"DEADBEEF")))
                      )
                    )
                  ),
                  Apply(Var("pkh"), LamAbs("hash", Var("hash")))
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
        // println(compiled.pretty.render(80))
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
        // println(compiled.pretty.render(80))
        val term = compiled.toUplc()
        // println(term.pretty.render(80))
        val evaled = VM.evaluateTerm(term)
        assert(evaled == scalus.uplc.Term.Const(Constant.ByteString(hex"deadbeef")))
    }

    test("compile match on ADT") {
        import scalus.prelude.List
        import scalus.prelude.List.*
        val compiled = compile {
            val ls: List[BigInt] = new Cons(BigInt(1), Nil)
            ls match
                case Cons(h, tl) => h
                case Nil         => BigInt(0)
        }
        // println(compiled.pretty.render(80))
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
        // println(evaled.pretty.render(80))
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
                new Cons((1, new TxOutRef(new TxId(hex"deadbeef"), 2)), Nil)
            ls match
                case Cons(h @ (a, TxOutRef(TxId(_), idx)), _) => a + idx
                case Nil                                      => BigInt(0)
        }
        // println(compiled.pretty.render(80))
        val term = compiled.toUplc()
        val evaled = VM.evaluateTerm(term)
        // println(evaled.pretty.render(80))
        assert(evaled == scalus.uplc.Term.Const(Constant.Integer(3)))
    }

    test("compile fieldAsData macro") {
        import scalus.ledger.api.v1.*
        import scalus.ledger.api.v1.ToDataInstances.given

        val compiled = compile { (ctx: scalus.builtin.Data) =>
            val sigsData = fieldAsData[ScriptContext](_.txInfo.signatories)(ctx)
            val sigs = Builtins.unListData(sigsData)
            Builtins.unBData(sigs.head)
        }
        // println(compiled.pretty.render(80))
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
        val evaled = VM.evaluateProgram(appliedScript)
        // println(evaled.pretty.render(80))
        assert(evaled == scalus.uplc.Term.Const(asConstant(hex"deadbeef")))
        val flatBytesLength = appliedScript.flatEncoded.length
        // println(Utils.bytesToHex(flatBytes))
        assert(flatBytesLength == 125)
    }

    test("@Ignore annotation") {
        @Ignore
        def foo() = 1

        assert(compile {
            @Ignore val a = true

            @Ignore def foo() = true
        } == Const(Constant.Unit))
    }

    test("Ignore PlatformSpecific arguments") {
        // Make sure that the implicit PlatformSpecific argument is not generated
        assert(compile(Builtins.sha2_256) == (lam("bs")(Sha2_256 $ Var("bs"))))
    }
