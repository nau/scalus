package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

class StateTransitionTest extends AnyFunSuite, ArbitraryInstances {
    private val params = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    test("not empty spent inputs validation rule success") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
              )
        )
        val result =
            for newState <- EmptinessLedgerValidator.Inputs.transit(LedgerState(), tx)
            yield newState
        assert(result.isRight)
    }

    test("not empty spent inputs validation rule failure") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(inputs = Set.empty)
        )
        val result =
            for newState <- EmptinessLedgerValidator.Inputs.transit(LedgerState(), tx)
            yield newState
        assert(result.isLeft)
    }

    test("spent inputs and reference inputs have empty intersection rule success") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                referenceInputs = None
              )
        )
        val result =
            for newState <- DisjointLedgerValidator.InputsAndReferenceInputs.transit(LedgerState(), tx)
            yield newState
        assert(result.isRight)
    }

    test("spent inputs and reference inputs have empty intersection rule failure") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(inputs = inputs, referenceInputs = Some(inputs))
        )
        val result =
            for newState <- DisjointLedgerValidator.InputsAndReferenceInputs.transit(LedgerState(), tx)
            yield newState
        assert(result.isLeft)
    }

    test("spent collateral reference inputs must be in utxo rule success") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                collateralInputs =
                    Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get),
                referenceInputs = Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get)
              )
        )
        val result =
            for newState <- MustBeInUtxoLedgerValidator.AllInputs
                    .transit(
                      LedgerState(
                        tx.body.inputs.view
                            .concat(tx.body.collateralInputs.getOrElse(Set.empty))
                            .concat(tx.body.referenceInputs.getOrElse(Set.empty))
                            .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
                            .toMap
                      ),
                      tx
                    )
            yield newState
        assert(result.isRight)
    }

    test("spent collateral reference inputs must be in utxo rule failure") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                collateralInputs =
                    Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get),
                referenceInputs = Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get)
              )
        )
        val result =
            for newState <- MustBeInUtxoLedgerValidator.AllInputs
                    .transit(LedgerState(), tx)
            yield newState
        assert(result.isLeft)
    }

    test("InputsLedgerSTM success") {
        val utxo: Utxo = Map.empty
        val env = UtxoEnv(slot = 100, params = params)
        val tx = randomValidTransaction.copy(
          body = Arbitrary
              .arbitrary[TransactionBody]
              .sample
              .get
              .copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                collateralInputs = None,
                referenceInputs = Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get)
              )
        )
        val result =
            for newState <- InputsLedgerSTS.transit(
                  LedgerState(
                    tx.body.inputs.view
                        .concat(tx.body.referenceInputs.getOrElse(Set.empty))
                        .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
                        .toMap
                  ),
                  tx
                )
            yield newState
        assert(result.isRight)
    }

    private def randomValidTransaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)
}
