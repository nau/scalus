package scalus.cardano.ledger
package rules

import org.scalacheck.Arbitrary
import scalus.cardano.address.{Address, ByronAddress}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedSet

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
                  inputs =
                      TaggedOrderedSet.from(Set(Arbitrary.arbitrary[TransactionInput].sample.get)),
                  collateralInputs = TaggedOrderedSet.empty,
                  referenceInputs = TaggedOrderedSet.empty,
                  outputs = IndexedSeq(
                    Sized(
                      TransactionOutput.Shelley(
                        Arbitrary.arbitrary[ByronAddress].sample.get,
                        Value(Coin(1000000L))
                      )
                    )
                  ),
                  votingProcedures = None,
                  proposalProcedures = TaggedOrderedSet.empty,
                  withdrawals = None,
                  certificates = TaggedSet.empty,
                  mint = None,
                  requiredSigners = TaggedOrderedSet.empty,
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
        val inputs = SortedSet.fill(1000) { // Arbitrary large number of inputs
            Arbitrary.arbitrary[TransactionInput].sample.get
        }

        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = TaggedOrderedSet(inputs)
                )
              )
            )
        }

        val result = TransactionSizeValidator.validate(context, State(), transaction)
        assert(result.isLeft)
    }
}
