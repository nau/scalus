package scalus.cardano.ledger
package rules

import org.scalatest.funsuite.AnyFunSuite

class InputsAndReferenceInputsDisjointValidatorTest extends AnyFunSuite, ValidatorRulesTestKit {
    test("InputsAndReferenceInputsDisjointValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(
                tx.body.value.copy(
                  inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                  referenceInputs = Set.empty
                )
              )
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(
          transaction.body.value.inputs.nonEmpty && transaction.body.value.referenceInputs.isEmpty
        )
    }

    test("InputsAndReferenceInputsDisjointValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
            val tx = randomValidTransaction
            tx.copy(
              body = KeepRaw(tx.body.value.copy(inputs = inputs, referenceInputs = inputs))
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(
          transaction.body.value.inputs.nonEmpty && transaction.body.value.referenceInputs.nonEmpty
        )
        assert(transaction.body.value.inputs == transaction.body.value.referenceInputs)
    }
}
