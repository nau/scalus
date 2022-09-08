package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.v1.{ScriptPurpose, TxId, TxInfo, TxOutRef, Value}
import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Data.*
import scalus.utils.Utils.*

import scala.collection.immutable

class DataDerivationSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("Simple derivation") {
    assert(TxId(hex"deadbeef").toData == hex"deadbeef".toData)
    assert(
      TxInfo(
        Nil,
        Nil,
        Value.zero,
        Value.zero,
        Nil,
        Nil,
        0,
        Nil,
        Nil,
        TxId(hex"bb")
      ).toData == Constr(
        0,
        immutable.List(
          List(Nil),
          List(Nil),
          List(Nil),
          List(Nil),
          List(Nil),
          List(Nil),
          I(0),
          List(Nil),
          List(Nil),
          B(hex"BB")
        )
      )
    )
    assert(ScriptPurpose.Minting(hex"deadbeef").toData == Constr(0, hex"deadbeef".toData :: Nil))
    assert(
      ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 2)).toData == Constr(
        1,
        Constr(0, hex"deadbeef".toData :: I(2) :: Nil) :: Nil
      )
    )
  }
