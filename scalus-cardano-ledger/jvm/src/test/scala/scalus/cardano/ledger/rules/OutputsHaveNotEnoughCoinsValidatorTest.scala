package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.cardano.address.{Address, ShelleyAddress, ShelleyPaymentPart}
import org.scalatest.funsuite.AnyFunSuite

class OutputsHaveNotEnoughCoinsValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("OutputsHaveNotEnoughCoinsValidator TransactionOutputs success") {
        val context = Context()

        val output = TransactionOutput(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.keyHash(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(Coin(1000000000L))
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

        val result = OutputsHaveNotEnoughCoinsValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveNotEnoughCoinsValidator TransactionOutputs failure") {
        val context = Context()

        val output = TransactionOutput(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.keyHash(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(Coin(1L))
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

        val result = OutputsHaveNotEnoughCoinsValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }

    test("OutputsHaveNotEnoughCoinsValidator CollateralReturnOutput success") {
        val context = Context()

        val collateralReturnOutput = TransactionOutput(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.keyHash(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(Coin(1000000000L))
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

        val result = OutputsHaveNotEnoughCoinsValidator.validate(context, state, transaction)
        assert(result.isRight)
    }

    test("OutputsHaveNotEnoughCoinsValidator CollateralReturnOutput failure") {
        val context = Context()

        val collateralReturnOutput = TransactionOutput(
          Arbitrary
              .arbitrary[ShelleyAddress]
              .sample
              .get
              .copy(payment =
                  ShelleyPaymentPart.keyHash(
                    Arbitrary.arbitrary[AddrKeyHash].sample.get
                  )
              ),
          Value(Coin(1L))
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

        val result = OutputsHaveNotEnoughCoinsValidator.validate(context, state, transaction)
        assert(result.isLeft)
    }
}
