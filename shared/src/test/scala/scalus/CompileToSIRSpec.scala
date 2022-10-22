package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.sir.{Binding, Recursivity}
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.uplc.DefaultFun.*
import scalus.uplc.ExprBuilder.compile
import scalus.uplc.TermDSL.{lam, λ}
import scalus.uplc.{Constant, DefaultFun, NamedDeBruijn}
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
    /*assert(compile {
      List[BigInt](1, 2, 3)
    } == Error("foo"))*/
    assert(
      compile {
        def asdf(l: List[BigInt]) = l.head
      } == Let(
        Rec,
        List(
          Binding("asdf", LamAbs("l", Apply(Builtin(HeadList), Var(NamedDeBruijn("l")))))
        ),
        Const(Constant.Unit)
      )
    )
  }
