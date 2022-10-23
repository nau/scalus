package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtins.Builtins
import scalus.ledger.api.v1.*
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.{Binding, Recursivity}
import scalus.uplc.DefaultFun.*
import scalus.uplc.ExprBuilder.compile
import scalus.uplc.TermDSL.{lam, λ}
import scalus.uplc.{Constant, Data, DefaultFun, DefaultUni, NamedDeBruijn}
import scalus.utils.Utils.*

import scala.collection.immutable

class CompileToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
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
    assert(compile(hex"deadbeef") == Const(Constant.ByteString(hex"deadbeef")))
  }

  test("compile if-then-else") {
    assert(
      compile {
        if true then () else ()
      } == Apply(
        Apply(
          Apply(Builtin(IfThenElse), Const(Constant.Bool(true))),
          Const(Constant.Unit)
        ),
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
        List(
          Binding(
            "a",
            Let(
              Rec,
              List(Binding("$anonfun", LamAbs("x", Var(NamedDeBruijn("x"))))),
              Var(NamedDeBruijn("$anonfun"))
            )
          )
        ),
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
        List.empty[BigInt]
      } == Const(Constant.List(DefaultUni.Integer, List()))
    )
    // Create a List literal
    assert(
      compile {
        List[BigInt](1, 2, 3)
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
        "bar" :: List(a)
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
        def head(l: List[BigInt]) = l.head
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
        def tail(l: List[BigInt]) = l.tail
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
        def isEmpty(l: List[BigInt]) = l.isEmpty
      } == Let(
        Rec,
        List(
          Binding("isEmpty", LamAbs("l", Apply(Builtin(NullList), Var(NamedDeBruijn("l")))))
        ),
        Const(Constant.Unit)
      )
    )

  }

/*
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
          Nil,
          Nil,
          TxId(hex"bb")
        ),
        ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
      )
    val compiled = compile {
      def validator(redeemer: Unit, datum: Unit, ctx: Data) =
        val txinfo = Builtins.unsafeDataAsList(Builtins.unsafeDataAsConstr(ctx).snd.head)
        val signatories = Builtins.unsafeDataAsList(txinfo.tail.tail.tail.tail.tail.tail.tail.head)
        def findSignatureOrFail(signatories: List[Data]): Unit =
          if signatories.isEmpty then throw new RuntimeException("Signature not found")
          // FIXME don't use Array[Byte], use ByteString
          else if Builtins.unsafeDataAsB(signatories.head) == hex"deadbeef" then ()
          else findSignatureOrFail(signatories.tail)
        findSignatureOrFail(signatories)

    }
    println(compiled)
  }
 */
