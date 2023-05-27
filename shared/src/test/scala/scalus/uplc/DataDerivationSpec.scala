package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.ledger.api.v1.Interval
import scalus.ledger.api.v1.ScriptPurpose
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.TxId
import scalus.ledger.api.v1.TxInfo
import scalus.ledger.api.v1.TxOutRef
import scalus.ledger.api.v1.Value
import scalus.uplc.Data.*
import scalus.uplc.ToDataInstances.given

import scala.collection.immutable

class DataDerivationSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances:
  test("Simple derivation") {
    assert(TxId(hex"deadbeef").toData == Constr(0, immutable.List(B(hex"deadbeef"))))
    assert(
      TxInfo(
        scalus.prelude.List.Nil,
        scalus.prelude.List.Nil,
        Value.zero,
        Value.zero,
        scalus.prelude.List.Nil,
        scalus.prelude.List.Nil,
        Interval.always,
        scalus.prelude.List.Nil,
        scalus.prelude.List.Nil,
        TxId(hex"bb")
      ).toData == Constr(
        0,
        immutable.List(
          List(Nil),
          List(Nil),
          Map(Nil),
          Map(Nil),
          List(Nil),
          List(Nil),
          Constr(
            0,
            immutable.List(
              Constr(0, immutable.List(Constr(0, Nil), Constr(1, Nil))),
              Constr(0, immutable.List(Constr(2, Nil), Constr(1, Nil)))
            )
          ),
          List(Nil),
          List(Nil),
          Constr(0, immutable.List(B(hex"BB")))
        )
      )
    )
    assert(ScriptPurpose.Minting(hex"deadbeef").toData == Constr(0, hex"deadbeef".toData :: Nil))
    assert(
      ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 2)).toData == Constr(
        1,
        Constr(0, Constr(0, immutable.List(hex"deadbeef".toData)) :: I(2) :: Nil) :: Nil
      )
    )
  }
