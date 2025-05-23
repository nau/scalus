package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ArbitraryInstances, Transaction}
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

class StateTransitionSpec extends AnyFunSuite, ArbitraryInstances {
    private val pparams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    test("simple rule") {
        pending
        val utxo: UTxO = Map.empty
        val env = UtxoEnv(slot = 100, pparams = pparams)
        val tx = Arbitrary.arbitrary[Transaction].sample.get
        ConwayTxValidation.validate(env, utxo, tx)
    }
}
