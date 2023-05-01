package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtins.ByteString.given
import scalus.builtins.{Builtins, ByteString}
import scalus.ledger.api.v1.*
import scalus.sir.Recursivity.*
import scalus.sir.SIR.*
import scalus.sir.{Binding, Recursivity, SIR, SimpleSirToUplcLowering}
import scalus.uplc.*
import scalus.uplc.DefaultFun.*
import scalus.Compiler.fieldAsData
import scalus.uplc.Compiler.compile
import scalus.uplc.TermDSL.{lam, λ}
import scalus.utils.Utils

import scala.collection.immutable
import scalus.Prelude.List.{Cons, Nil}
import scalus.Prelude
import scalus.Prelude.{===, given}
import scalus.sir.DataDecl
import scalus.sir.ConstrDecl

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  val deadbeef = Constant.ByteString(hex"deadbeef")

  test("compile external definitions") {
    import TestCode.foo
    compile {
      foo()
    }
  }
