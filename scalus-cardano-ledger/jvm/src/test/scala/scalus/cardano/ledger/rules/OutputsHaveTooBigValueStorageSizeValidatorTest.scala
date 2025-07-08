package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.cardano.address.{Address, ShelleyAddress}
import org.scalatest.funsuite.AnyFunSuite

class OutputsHaveTooBigValueStorageSizeValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("OutputsHaveTooBigValueStorageSizeValidator TransactionOutputs success") {
        val context = Context()

        val output = Arbitrary.arbitrary[TransactionOutput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq(Sized(output)),
                  collateralReturnOutput = None
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator TransactionOutputs failure") {
        val context = Context()

        val output = TransactionOutput.Shelley(
          Address.Shelley(
            Arbitrary.arbitrary[ShelleyAddress].sample.get
          ),
          Value(
            Coin(1L),
            genMultiAsset(
              minPolicies = 10,
              maxPolicies = 10,
              minAssets = 100,
              maxAssets = 100
            ).sample.get
          )
        )

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq(Sized(output)),
                  collateralReturnOutput = None
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator CollateralReturnOutput success") {
        val context = Context()

        val collateralReturnOutput = Arbitrary.arbitrary[TransactionOutput].sample.get

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq.empty,
                  collateralReturnOutput = Some(Sized(collateralReturnOutput))
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveTooBigValueStorageSizeValidator CollateralReturnOutput failure") {
        val context = Context()

        val collateralReturnOutput = TransactionOutput.Shelley(
          Address.Shelley(
            Arbitrary.arbitrary[ShelleyAddress].sample.get
          ),
          Value(
            Coin(1L),
            genMultiAsset(
              minPolicies = 10,
              maxPolicies = 10,
              minAssets = 100,
              maxAssets = 100
            ).sample.get
          )
        )

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  outputs = IndexedSeq.empty,
                  collateralReturnOutput = Some(Sized(collateralReturnOutput))
                )
              )
            )
        }

        val state = State()

        val result =
            OutputsHaveTooBigValueStorageSizeValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
