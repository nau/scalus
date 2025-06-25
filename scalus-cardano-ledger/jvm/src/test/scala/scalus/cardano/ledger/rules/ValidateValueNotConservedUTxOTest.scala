package scalus.cardano.ledger.rules

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.rules.ValidateValueNotConservedUTxO.validate

class ValidateValueNotConservedUTxOTest extends AnyFunSuite, ArbitraryInstances {

    test("value is preserved in an empty transaction") {
        val context = Context()
        val tx = randomValidTransaction.copy(
          body = KeepRaw(
            TransactionBody(
              inputs = Set.empty,
              outputs = IndexedSeq.empty,
              fee = Coin.zero,
            )
          )
        )

        assert(validate(context, State(), tx).isRight, "Empty transaction should preserve value")
    }

    test("value is not preserved when inputs and outputs are not equal") {
        val context = Context()
        val input = TransactionInput(
          transactionId = randomValidTransaction.id,
          index = 0
        )
        val resolvedOutput = TransactionOutput.Babbage(
          address = arbitrary[Address].sample.get,
          value = Value.lovelace(123),
        )
        val output = TransactionOutput.Babbage(
          address = arbitrary[Address].sample.get,
          value = Value.lovelace(10000000), // 1 ADA
        )
        val state = State(
          utxo = Map(input -> resolvedOutput)
        )
        val tx = randomValidTransaction.copy(
          body = KeepRaw(
            randomValidTransaction.body.value.copy(
              fee = Coin.zero,
              mint = None,
              inputs = Set(input),
              outputs = IndexedSeq(output)
            )
          )
        )

        assert(validate(context, state, tx).isLeft)
    }

    private def randomValidTransaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)
}
