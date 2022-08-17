package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.v2.{ScriptPurpose, TxId, TxInfo, TxOutRef}
import scalus.ledger.api.v2.Instances.given
import scalus.uplc.Data.*
import scalus.utils.Utils.*

class DataDerivationSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("Simple derivation") {
    assert(TxId(hex"deadbeef").toData == Constr(0, hex"deadbeef".toData :: Nil))
    assert(TxInfo(Nil, Nil, Nil).toData == Constr(0, List(Nil) :: List(Nil) :: List(Nil) :: Nil))
    assert(ScriptPurpose.Minting(hex"deadbeef").toData == Constr(0, hex"deadbeef".toData :: Nil))
    assert(
      ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 2)).toData == Constr(
        1,
        Constr(0, Constr(0, hex"deadbeef".toData :: Nil) :: I(2) :: Nil) :: Nil
      )
    )
  }
