package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.cardano.address.{Address, ByronAddress}
import org.scalatest.funsuite.AnyFunSuite

class TransactionSizeValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("TransactionSizeValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              witnessSet = tx.witnessSet.copy(
                vkeyWitnesses = Set.empty,
                bootstrapWitnesses = Set.empty,
                nativeScripts = Set.empty,
                plutusV1Scripts = Set.empty,
                plutusV2Scripts = Set.empty,
                plutusV3Scripts = Set.empty,
                plutusData = KeepRaw(TaggedSet.empty),
                redeemers = None
              ),
              auxiliaryData = None,
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = Set(Arbitrary.arbitrary[TransactionInput].sample.get),
                  collateralInputs = Set.empty,
                  referenceInputs = Set.empty,
                  outputs = IndexedSeq(
                    Sized(
                      TransactionOutput.Shelley(
                        Address.Byron(Arbitrary.arbitrary[ByronAddress].sample.get),
                        Value(Coin(1000000L))
                      )
                    )
                  ),
                  votingProcedures = None,
                  proposalProcedures = Set.empty,
                  withdrawals = None,
                  certificates = TaggedSet.empty,
                  mint = None,
                  requiredSigners = Set.empty,
                  collateralReturnOutput = None
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
        assert(result.isRight)
    }

    test("TransactionSizeValidator rule failure") {
        val context = Context()
        val inputs = Set.fill(1000) { // Arbitrary large number of inputs
            Arbitrary.arbitrary[TransactionInput].sample.get
        }

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = inputs
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
        assert(result.isLeft)
    }
}
