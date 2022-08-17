package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.ledger.api.v2.TxId
import scalus.uplc.Data.*
import scalus.utils.Utils.*

class DataDerivationSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("Simple derivation") {
    assert(TxId(hex"deadbeef").toData == Constr(0, hex"deadbeef".toData :: Nil))
  }
