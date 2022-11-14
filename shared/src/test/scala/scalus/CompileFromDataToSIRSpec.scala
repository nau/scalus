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
import scalus.uplc.ExprBuilder.{compile, fieldAsData1}
import scalus.uplc.TermDSL.{lam, λ}
import scalus.utils.Utils

import scala.collection.immutable

class CompileFromDataToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  val deadbeef = Constant.ByteString(hex"deadbeef")

  inline def compilesTo(expected: SIR)(inline e: Any) = assert(compile(e) == expected)

  /* test("compile FromData[Boolean]") {
    val compiled = compile {
      summon[Data.FromData[Boolean]]
        .fromData(Builtins.mkConstr(0, scalus.builtins.List.empty[Data]))
    }
    println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    println(term.pretty.render(80))
    val flatBytes = ProgramFlatCodec.encodeFlat(Program(version = (1, 0, 0), term = term))
    println(flatBytes.length)
  } */

  test("compile FromData[(A, B)]") {
    val compiled = compile {
      val t = Builtins.mkConstr(1, scalus.builtins.List.empty[Data])
      val f = Builtins.mkConstr(0, scalus.builtins.List.empty[Data])
      summon[Data.FromData[(Boolean, Boolean)]](
        Builtins.mkConstr(
          0,
          scalus.builtins.List(f, t)
        )
      )
    }
    println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    println(term.pretty.render(80))
    val flatBytes = ProgramFlatCodec.encodeFlat(Program(version = (1, 0, 0), term = term))
    println(flatBytes.length)
  }
