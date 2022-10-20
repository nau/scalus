package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.sir.{Binding, Recursivity}
import scalus.sir.SIR.*
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
          Apply(Builtin(DefaultFun.IfThenElse), Const(Constant.Bool(true))),
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

  /*test("compile lambda") {
    assert(
      compile {
        val a = (x: Boolean) => x
        a(true)
      } == Apply(
        Apply(
          Apply(Builtin(DefaultFun.IfThenElse), Const(Constant.Bool(true))),
          Const(Constant.Unit)
        ),
        Const(Constant.Unit)
      )
    )
  }*/
