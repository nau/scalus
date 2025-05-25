package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.Ignore
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{ArbitraryInstances, Coin, Transaction}
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

@Ignore
class StateTransitionTest extends AnyFunSuite, ArbitraryInstances {
    private val pparams = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    test("simple rule") {
        val utxo: UTxO = Map.empty
        val env = UtxoEnv(slot = 100, pparams = pparams)
        val tx = Arbitrary.arbitrary[Transaction].sample.get
        val result =
            for newState <- ConwayTxValidation.validate(env, utxo, tx)
            yield newState
        assert(result.isRight)
    }

    test("fees validation rule success") {
        val utxo: UTxO = Map.empty
        val env = UtxoEnv(slot = 100, pparams = pparams)
        val tx = Arbitrary.arbitrary[Transaction].sample.get
        val result =
            for newState <- FeesValidation.validate(env, utxo, tx)
            yield newState
        assert(result.isRight)
    }

    test("fees validation rule failure") {
        val utxo: UTxO = Map.empty
        val env = UtxoEnv(slot = 100, pparams = pparams)
        val tx =
            val tx = Arbitrary.arbitrary[Transaction].sample.get
            tx.copy(body = tx.body.copy(fee = Coin(0)))
        val result =
            for newState <- FeesValidation.validate(env, utxo, tx)
            yield newState
        assert(result.isLeft)
    }
}
