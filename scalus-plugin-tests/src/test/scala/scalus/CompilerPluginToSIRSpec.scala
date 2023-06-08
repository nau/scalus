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
import scalus.uplc.ToData.toData
import scalus.utils.Utils

import scala.collection.immutable
import scalus.flat.DecoderState
import scalus.uplc.ToData

@Compile
object TotoDataInstances {
  given ToData.ToData[BigInt] = (a: BigInt) => builtins.Builtins.mkI(a)
  given ToData.ToData[String] = (a: String) => builtins.Builtins.mkI(1)
}

case class Test(a: BigInt, b: String)
@Compile
object Test {
  import TotoDataInstances.given
  given ToData.ToData[Test] = ToData.deriveCaseClass[Test](12)
}

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  val deadbeef = Constant.ByteString(hex"deadbeef")

  test("compile literals") {
    val sir = compile {
      val a = new Test(1, "asdf").toData
      a
    }
    println(sir.pretty.render(100))
  }
