package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.*
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.sir.Binding
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Recursivity
import scalus.sir.Recursivity.*
import scalus.sir.SIR
import scalus.sir.SIR.*
import scalus.sir.SimpleSirToUplcLowering
import scalus.sir.SirDSL.{_, given}
import scalus.uplc.DefaultFun.*
import scalus.uplc.TermDSL.lam
import scalus.uplc.TermDSL.λ
import scalus.uplc.*
import scalus.utils.Utils

import scala.collection.immutable

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
      compile(builtins.ByteString.empty) == Const(Constant.ByteString(builtins.ByteString.empty))
    )

    assert(
      compile(builtins.ByteString.fromHex("deadbeef")) == Const(deadbeef)
    )
  }

  test("compile if-then-else") {
    assert(
      compile {
        if scalus.builtins.Builtins.equalsInteger(1, 2) then () else ()
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
//        b()
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
    assert(compile { throw new RuntimeException("foo") } == Error("foo"))
  }

  /* test("compile ToData") {
    import scalus.uplc.Data.*
    val compiled = compile {
      BigInt(1).toData
    }
    assert(
      compiled == Let(
        Rec,
        immutable.List(
          Binding(
            "scalus.uplc.Data$.given_ToData_BigInt$.toData",
            LamAbs("a", Apply(Builtin(IData), Var("a")))
          )
        ),
        Let(
          NonRec,
          immutable.List(Binding("a$proxy1", Const(Constant.Integer(1)))),
          Apply(
            Var("scalus.uplc.Data$.given_ToData_BigInt$.toData"),
            Var("a$proxy1")
          )
        )
      )
    )
//    val term = new SimpleSirToUplcLowering().lower(compiled)
//    assert(Cek.evalUPLC(term) == Data.I(22))
  } */

  test("compile List builtins") {
    assert(
      compile(
        Builtins.chooseList(builtins.List[BigInt](1, 2, 3), true, false)
      ) == (DefaultFun.ChooseList $ List(1, 2, 3) $ true $ false)
    )
    assert(
      compile(
        Builtins.mkCons(BigInt(4), builtins.List[BigInt](1, 2, 3))
      ) == (DefaultFun.MkCons $ 4 $ List(1, 2, 3))
    )
    assert(
      compile(
        Builtins.headList(builtins.List[BigInt](1, 2, 3))
      ) == (DefaultFun.HeadList $ List(1, 2, 3))
    )
    assert(
      compile(
        Builtins.tailList(builtins.List[BigInt](1, 2, 3))
      ) == (DefaultFun.TailList $ List(1, 2, 3))
    )
    assert(
      compile(
        Builtins.nullList(builtins.List[BigInt](1, 2, 3))
      ) == (DefaultFun.NullList $ List(1, 2, 3))
    )

    // Nil
    assert(
      compile {
        builtins.List.empty[BigInt]
      } == Const(Constant.List(DefaultUni.Integer, List()))
    )
    // Create a List literal
    assert(
      compile {
        builtins.List[BigInt](1, 2, 3)
      } == Const(
        Constant.List(
          DefaultUni.Integer,
          List(Constant.Integer(1), Constant.Integer(2), Constant.Integer(3))
        )
      )
    )

    // MkCons builtin
    assert(
      compile {
        val a = "foo"
        "bar" :: builtins.List(a)
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

    assert(
      compile {
        def head(l: builtins.List[BigInt]) = l.head
      } == Let(
        Rec,
        List(
          Binding("head", LamAbs("l", Apply(Builtin(HeadList), Var("l"))))
        ),
        Const(Constant.Unit)
      )
    )
    assert(
      compile {
        def tail(l: builtins.List[BigInt]) = l.tail
      } == Let(
        Rec,
        List(
          Binding("tail", LamAbs("l", Apply(Builtin(TailList), Var("l"))))
        ),
        Const(Constant.Unit)
      )
    )
    assert(
      compile {
        def isEmpty(l: builtins.List[BigInt]) = l.isEmpty
      } == Let(
        Rec,
        List(
          Binding("isEmpty", LamAbs("l", Apply(Builtin(NullList), Var("l"))))
        ),
        Const(Constant.Unit)
      )
    )
    assert(compile(Builtins.mkNilData) == (Builtin(MkNilData)))
    assert(compile(Builtins.mkNilPairData) == (Builtin(MkNilPairData)))
  }

  test("compile Data builtins") {
    val nilData = Const(Constant.List(DefaultUni.Data, immutable.Nil))
    assert(
      compile {
        Builtins.mkConstr(
          1,
          builtins.List(Builtins.mkI(2))
        )
      } == Apply(
        Apply(Builtin(ConstrData), Const(Constant.Integer(1))),
        Apply(
          Apply(Builtin(MkCons), Apply(Builtin(IData), Const(Constant.Integer(2)))),
          nilData
        )
      )
    )
    assert(
      compile {
        Builtins.mkList(builtins.List(Builtins.mkI(1)))
      } ==
        Apply(
          Builtin(ListData),
          Apply(
            Apply(Builtin(MkCons), Apply(Builtin(IData), Const(Constant.Integer(1)))),
            nilData
          )
        )
    )
    assert(
      compile {
        Builtins.mkMap(
          builtins.List(
            builtins.Pair(Builtins.mkB(ByteString.fromHex("deadbeef")), Builtins.mkI(1))
          )
        )
      } == Apply(
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

    assert(
      compile {
        def unb(d: Data) = Builtins.unsafeDataAsConstr(d)
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
    assert(
      compile {
        def unb(d: Data) = Builtins.unsafeDataAsList(d)
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
    assert(
      compile {
        def unb(d: Data) = Builtins.unsafeDataAsMap(d)
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
    assert(
      compile {
        def unb(d: Data) = Builtins.unsafeDataAsB(d)
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
    assert(
      compile {
        def unb(d: Data) = Builtins.unsafeDataAsI(d)
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
    // ChooseData
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
    // EqualsData
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
    // SerialiseData
    assert(compile { Builtins.serialiseData } == (Builtin(DefaultFun.SerialiseData)))
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
        Builtins.appendByteString(ByteString.fromHex("dead"), ByteString.fromHex("beef"))
      ) == (AppendByteString $ hex"dead" $ hex"beef")
    )

    assert(
      compile(
        Builtins.sliceByteString(ByteString.fromHex("dead"), 1, 2)
      ) == (SliceByteString $ hex"dead" $ 1 $ 2)
    )

    assert(
      compile(
        Builtins.lengthOfByteString(ByteString.fromHex("dead"))
      ) == (LengthOfByteString $ hex"dead")
    )

    assert(
      compile(
        Builtins.indexByteString(ByteString.fromHex("dead"), 1)
      ) == (IndexByteString $ hex"dead" $ 1)
    )

    assert(
      compile(
        Builtins.equalsByteString(ByteString.fromHex("dead"), ByteString.fromHex("beef"))
      ) == (EqualsByteString $ hex"dead" $ hex"beef")
    )

    assert(
      compile(
        Builtins.lessThanByteString(ByteString.fromHex("dead"), ByteString.fromHex("beef"))
      ) == (LessThanByteString $ hex"dead" $ hex"beef")
    )

    assert(
      compile(
        Builtins.lessThanEqualsByteString(ByteString.fromHex("dead"), ByteString.fromHex("beef"))
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
    assert(compile(Builtins.sha2_256(ByteString.fromHex("dead"))) == (Sha2_256 $ hex"dead"))
    assert(compile(Builtins.sha3_256(ByteString.fromHex("dead"))) == (Sha3_256 $ hex"dead"))
    assert(compile(Builtins.blake2b_256(ByteString.fromHex("dead"))) == (Blake2b_256 $ hex"dead"))
    assert(
      compile(
        Builtins.verifyEd25519Signature(
          ByteString.fromHex("dead"),
          ByteString.fromHex("beef"),
          ByteString.fromHex("cafe")
        )
      ) == (VerifyEd25519Signature $ hex"dead" $ hex"beef" $ hex"cafe")
    )
    assert(
      compile(
        Builtins.verifyEcdsaSecp256k1Signature(
          ByteString.fromHex("dead"),
          ByteString.fromHex("beef"),
          ByteString.fromHex("cafe")
        )
      ) == (VerifyEcdsaSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
    )
    assert(
      compile(
        Builtins.verifySchnorrSecp256k1Signature(
          ByteString.fromHex("dead"),
          ByteString.fromHex("beef"),
          ByteString.fromHex("cafe")
        )
      ) == (VerifySchnorrSecp256k1Signature $ hex"dead" $ hex"beef" $ hex"cafe")
    )
  }

  test("compile String builtins") {
    assert(compile(Builtins.appendString("dead", "beef")) == (AppendString $ "dead" $ "beef"))
    assert(compile(Builtins.equalsString("dead", "beef")) == (EqualsString $ "dead" $ "beef"))
    assert(compile(Builtins.encodeUtf8("dead")) == (EncodeUtf8 $ "dead"))
    assert(compile(Builtins.decodeUtf8(ByteString.fromHex("dead"))) == (DecodeUtf8 $ hex"dead"))
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
        def swap(p: builtins.Pair[Data, Data]) =
          builtins.Pair(Builtins.sndPair(p), Builtins.fstPair(p))
      } == (
        Let(
          Rec,
          List(
            Binding("swap", LamAbs("p", MkPairData $ (SndPair $ Var("p")) $ (FstPair $ Var("p"))))
          ),
          Const(Constant.Unit)
        )
      )
    )
    assert(compile {
      builtins.Pair(BigInt(1), ByteString.fromHex("deadbeef"))
    } == (Const(Constant.Pair(Constant.Integer(1), deadbeef))))
    assert(
      compile {
        def swap(p: builtins.Pair[Data, Data]) = builtins.Pair(p.snd, p.fst)
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
      val a = true
      !a && false || true
    }
    assert(
      compiled ==
        Let(
          NonRec,
          List(Binding("a", Const(Bool(true)))),
          Or(And(Not(Var("a")), Const(Bool(false))), Const(Bool(true)))
        )
    )
    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    val evaled = Cek.evalUPLC(term)
    // println(evaled.pretty.render(80))
    assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
  }

  test("compile type-safe equality") {
    import scalus.prelude.Prelude.*
    val compiled = compile {
      val a = BigInt(0)
      val bs = ByteString.fromHex("deadbeef")
      val s = "string"
      a === a && bs === bs && s === s
    }
    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    // println(term.pretty.render(80))
    val evaled = Cek.evalUPLC(term)
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
      val pkh = new scalus.ledger.api.v1.PubKeyHash(ByteString.fromHex("deadbeef"))
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
                  List(Const(uplc.Constant.ByteString(ByteString.fromHex("DEADBEEF"))))
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
      val t = (true, false)
      t match
        case (a, b) => a && t._2
    }
    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    val evaled = Cek.evalUPLC(term)
    assert(evaled == scalus.uplc.Term.Const(Constant.Bool(false)))
  }

  test("compile match on a case class") {
    val compiled = compile {
      val pkh = new scalus.ledger.api.v1.PubKeyHash(ByteString.fromHex("deadbeef"))
      pkh match
        case PubKeyHash(hash) => hash
    }
    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    // println(term.pretty.render(80))
    val evaled = Cek.evalUPLC(term)
    assert(evaled == scalus.uplc.Term.Const(Constant.ByteString(ByteString.fromHex("deadbeef"))))
  }

  test("compile match on ADT") {
    import scalus.ledger.api.v1.*
    import scalus.prelude.List
    import scalus.prelude.List.*
    val compiled = compile {
      val ls: List[BigInt] = new Cons(BigInt(1), List.Nil)
      ls match
        case Cons(h, tl) => h
        case Nil         => BigInt(0)
    }
    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    val evaled = Cek.evalUPLC(term)
    // println(evaled.pretty.render(80))
    assert(evaled == scalus.uplc.Term.Const(Constant.Integer(1)))
  }

  test("compile fieldAsData macro") {
    import scalus.ledger.api.v1.{*, given}
    val compiled = compile { (ctx: scalus.uplc.Data) =>
      val sigsData = fieldAsData[ScriptContext](_.scriptContextTxInfo.txInfoSignatories)(ctx)
      val sigs = Builtins.unsafeDataAsList(sigsData)
      Builtins.unsafeDataAsB(sigs.head)
    }
    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)

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
    import scalus.uplc.Data.{*}
    import DefaultUni.asConstant
    val appliedScript = Program(version = (1, 0, 0), term = term $ scriptContext.toData)
    val evaled = Cek.evalUPLCProgram(appliedScript)
    // println(evaled.pretty.render(80))
    assert(evaled == scalus.uplc.Term.Const(asConstant(hex"deadbeef")))
    val flatBytesLength = ProgramFlatCodec.encodeFlat(appliedScript).length
    // println(Utils.bytesToHex(flatBytes))
    assert(flatBytesLength == 125)
  }
