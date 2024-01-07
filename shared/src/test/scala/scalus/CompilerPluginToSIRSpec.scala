package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.builtins.given
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
import scalus.uplc.*

import scala.collection.immutable

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  val deadbeef = Constant.ByteString(hex"deadbeef")

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
    val evaled = Cek.evalUPLC(term)
    // println(evaled.pretty.render(80))
    assert(evaled == scalus.uplc.Term.Const(Constant.Bool(true)))
  }

