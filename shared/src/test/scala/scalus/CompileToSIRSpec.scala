package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtins.{Builtins, ByteString}
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.*
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.{Binding, Recursivity, SIR, SimpleSirToUplcLowering}
import scalus.uplc.DefaultFun.*
import scalus.uplc.ExprBuilder.compile
import scalus.uplc.TermDSL.{lam, λ}
import scalus.uplc.*
import scalus.utils.Utils

import scala.collection.immutable

class CompileToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  val deadbeef = Constant.ByteString(hex"deadbeef")

  inline def compilesTo(expected: SIR)(inline e: Any) = assert(compile(e) == expected)

  test("compile literals") {
    assert(compile(false) == Const(Constant.Bool(false)))
    assert(compile(true) == Const(Constant.Bool(true)))
    assert(compile(()) == Const(Constant.Unit)) // FIXME
    assert(compile("foo") == Const(Constant.String("foo")))
    assert(
      compile(BigInt("15511210043330985984000000")) == Const(
        Constant.Integer(BigInt("15511210043330985984000000"))
      )
    )
    assert(compile(12: BigInt) == Const(Constant.Integer(BigInt("12"))))
    // ByteStrings
    assert(
      compile(builtins.ByteString.empty) == Const(Constant.ByteString(builtins.ByteString.empty))
    )
    assert(
      compile(builtins.ByteString.fromHex("deadbeef")) == Const(deadbeef)
    )
    assert(
      compile(
        builtins.ByteString.unsafeFromArray(
          Array(0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte)
        )
      ) == Const(deadbeef)
    )
    assert(
      compile(
        builtins.ByteString(
          Array(0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte)
        )
      ) == Const(deadbeef)
    )
    assert(
      compile(
        builtins.ByteString(0xde.toByte, 0xad.toByte, 0xbe.toByte, 0xef.toByte)
      ) == Const(deadbeef)
    )

  }

  test("compile if-then-else") {
    assert(
      compile {
        if true then () else ()
      } == SIR.IfThenElse(Const(Constant.Bool(true)), Const(Constant.Unit), Const(Constant.Unit))
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
        Var(NamedDeBruijn("a"))
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
          immutable.List(Binding("c", LamAbs("x", Var(NamedDeBruijn("x"))))),
          Apply(Var(NamedDeBruijn("c")), Apply(Var(NamedDeBruijn("b")), Const(Constant.Unit)))
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
        List(Binding("a", LamAbs("x", Var(NamedDeBruijn("x"))))),
        Apply(Var(NamedDeBruijn("a")), Const(Constant.Bool(true)))
      )
    )
  }

  test("compile throw") {
    assert(compile { throw new RuntimeException("foo") } == Error("foo"))
  }

  test("compile List builtins") {
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
            Apply(Builtin(MkCons), Var(NamedDeBruijn("a"))),
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
          Binding("head", LamAbs("l", Apply(Builtin(HeadList), Var(NamedDeBruijn("l")))))
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
          Binding("tail", LamAbs("l", Apply(Builtin(TailList), Var(NamedDeBruijn("l")))))
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
          Binding("isEmpty", LamAbs("l", Apply(Builtin(NullList), Var(NamedDeBruijn("l")))))
        ),
        Const(Constant.Unit)
      )
    )
  }

  test("compile Data builtins") {
    val nilData = Const(Constant.List(DefaultUni.Data, immutable.Nil))
    assert(
      compile {
        Builtins.mkConstr(
          1,
          builtins.List(Data.I(2))
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
        Builtins.mkList(builtins.List(Data.I(1)))
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
          builtins.List(builtins.Pair(Data.B(ByteString.fromHex("deadbeef")), Data.I(1)))
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
            LamAbs("d", Apply(Builtin(DefaultFun.UnConstrData), Var(NamedDeBruijn("d"))))
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
            LamAbs("d", Apply(Builtin(DefaultFun.UnListData), Var(NamedDeBruijn("d"))))
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
            LamAbs("d", Apply(Builtin(DefaultFun.UnMapData), Var(NamedDeBruijn("d"))))
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
            LamAbs("d", Apply(Builtin(DefaultFun.UnBData), Var(NamedDeBruijn("d"))))
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
            LamAbs("d", Apply(Builtin(DefaultFun.UnIData), Var(NamedDeBruijn("d"))))
          )
        ),
        Const(Constant.Unit)
      )
    )
  }

  test("compile Pair builtins") {
    compilesTo(Const(Constant.Pair(Constant.Integer(1), deadbeef))) {
      builtins.Pair(BigInt(1), ByteString.fromHex("deadbeef"))
    }
    compilesTo(
      Let(
        Rec,
        List(
          Binding(
            "swap",
            LamAbs(
              "p",
              Apply(
                Apply(Builtin(MkPairData), Apply(Builtin(SndPair), Var(NamedDeBruijn("p")))),
                Apply(Builtin(FstPair), Var(NamedDeBruijn("p")))
              )
            )
          )
        ),
        Const(Constant.Unit)
      )
    ) {
      def swap(p: builtins.Pair[Data, Data]) = builtins.Pair(p.snd, p.fst)
    }
  }

  test("compile external definitions") {
    def foo(i: BigInt) = i
    compilesTo(
      Let(
        Rec,
        List(Binding("foo", LamAbs("i", Var(NamedDeBruijn("i"))))),
        Apply(Var(NamedDeBruijn("foo")), Const(Constant.Integer(5)))
      )
    ) {
      foo(5)
    }
  }

  test("PubKey Validator example") {
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
          PubKeyHash(hex"deadbeef") :: Nil,
          Nil,
          TxId(hex"bb")
        ),
        ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
      )

    def validator(redeemer: Unit, datum: Unit, ctx: Data) = {
      val txinfo = Builtins.unsafeDataAsConstr(Builtins.unsafeDataAsConstr(ctx).snd.head).snd
      val signatories = Builtins.unsafeDataAsList(txinfo.tail.tail.tail.tail.tail.tail.tail.head)

      def findSignatureOrFail(signatories: builtins.List[Data]): Unit =
        if signatories.isEmpty then throw new RuntimeException("Signature not found")
        else if Builtins.unsafeDataAsB(signatories.head) == ByteString.fromHex("deadbeef") then ()
        else findSignatureOrFail(signatories.tail)

      findSignatureOrFail(signatories)
    }

    val compiled = compile { validator }

//    println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    val flatBytes = ProgramFlatCodec.encodeFlat(Program(version = (1, 0, 0), term = term))
//    println(Utils.bytesToHex(flatBytes))
    assert(flatBytes.length == 133)
//    println(term.pretty.render(80))
    import TermDSL.{*, given}
    import Data.*
    import DefaultUni.asConstant
//    println(scriptContext.toData)
    val appliedValidator = term $ asConstant(()) $ asConstant(()) $ scriptContext.toData
    assert(
      Cek.evalUPLC(appliedValidator) == Term.Const(asConstant(()))
    )
    assert(
      validator((), (), scriptContext.toData) == ()
    )
  }
