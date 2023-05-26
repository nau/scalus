package scalus.ledger.api.v1

import org.scalacheck.Arbitrary
import org.scalacheck.Prop
import org.scalacheck.Properties
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.Checkers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.pretty
import scalus.uplc.ArbitraryInstances

import scala.collection.immutable

import scalus.prelude.Prelude.{*, given}
import scalus.prelude.Maybe.*
import scalus.Compiler.compile
import scalus.sir.SIR
import scalus.uplc.Term
import scalus.uplc.TermDSL.{*, given}
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Cek
import scalus.builtins.ByteString

class ContextSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
  test("PubKeyHash Eq") {
    assert(PubKeyHash(ByteString.empty) === PubKeyHash(ByteString.empty))
    assert(PubKeyHash(ByteString.fromHex("aa")) !== PubKeyHash(ByteString.empty))
    assertEval(compile(new PubKeyHash(ByteString.empty) === new PubKeyHash(ByteString.empty)), true)
    assertEval(
      compile(new PubKeyHash(ByteString.fromHex("aa")) === new PubKeyHash(ByteString.empty)),
      false
    )
    assertEval(
      compile(
        new PubKeyHash(ByteString.fromHex("aa")) === new PubKeyHash(ByteString.fromHex("aa"))
      ),
      true
    )
  }

  private def assertEval(sir: SIR, expected: Term) = {
    val term = new SimpleSirToUplcLowering().lower(sir)
    assert(Cek.evalUPLC(term) == expected)
  }
}
