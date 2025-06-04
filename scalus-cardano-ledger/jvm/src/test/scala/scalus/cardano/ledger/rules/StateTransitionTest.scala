package scalus.cardano.ledger.rules

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.*
import scalus.ledger.babbage.ProtocolParams
import upickle.default.read

class StateTransitionTest extends AnyFunSuite, ArbitraryInstances {
    private val params = read[ProtocolParams](
      this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
    )(using ProtocolParams.blockfrostParamsRW)

    test("EmptyInputsValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
              )
            )
        }

        val result = EmptyInputsValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty)
    }

    test("EmptyInputsValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set.empty
              )
            )
        }

        val result = EmptyInputsValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.inputs.isEmpty)
    }

    test("InputsAndReferenceInputsDisjointValidator rule success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                referenceInputs = None
              )
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty && transaction.body.referenceInputs.isEmpty)
    }

    test("InputsAndReferenceInputsDisjointValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(inputs = inputs, referenceInputs = Some(inputs))
            )
        }

        val result = InputsAndReferenceInputsDisjointValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.inputs.nonEmpty && transaction.body.referenceInputs.nonEmpty)
        assert(transaction.body.inputs == transaction.body.referenceInputs.get)
    }

    test("AllInputsMustBeInUtxoValidator rule success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                collateralInputs =
                    Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get),
                referenceInputs = Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get)
              )
            )
        }
        val state = State(
          utxo = transaction.body.inputs.view
              .concat(transaction.body.collateralInputs.get)
              .concat(transaction.body.referenceInputs.get)
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.forall(state.utxo.contains))
        assert(transaction.body.collateralInputs.get.forall(state.utxo.contains))
        assert(transaction.body.referenceInputs.get.forall(state.utxo.contains))
    }

    test("AllInputsMustBeInUtxoValidator rule failure") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                collateralInputs =
                    Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get),
                referenceInputs = Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get)
              )
            )
        }

        val result = AllInputsMustBeInUtxoValidator.validate(context, state, transaction)
        assert(result.isLeft)
        assert(!transaction.body.inputs.forall(state.utxo.contains))
        assert(!transaction.body.collateralInputs.get.forall(state.utxo.contains))
        assert(!transaction.body.referenceInputs.get.forall(state.utxo.contains))
    }

    test("EqualValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount rule success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[AddressBytes].sample.get,
                    Value.Ada(Coin(Gen.choose(0L, 1000000L).sample.get))
                  )
                ),
                fee = Coin(Gen.choose(0L, 1000000L).sample.get)
              )
            )
        }
        val state = State(
          utxo = Map(
            transaction.body.inputs.head -> TransactionOutput.Shelley(
              Arbitrary.arbitrary[AddressBytes].sample.get,
              Value.Ada(
                Coin(
                  transaction.body.outputs.head
                      .asInstanceOf[TransactionOutput.Shelley]
                      .value
                      .asInstanceOf[Value.Ada]
                      .coin
                      .value +
                      transaction.body.fee.value
                )
              )
            )
          )
        )

        val result = EqualValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
          context,
          state,
          transaction
        )
        assert(result.isRight)
    }

    test("EqualValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount rule failure") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[AddressBytes].sample.get,
                    Value.Ada(Coin(Gen.choose(0L, 1000000L).sample.get))
                  )
                ),
                fee = Coin(Gen.choose(0L, 1000000L).sample.get)
              )
            )
        }
        val state = State(
          utxo = Map(
            transaction.body.inputs.head -> TransactionOutput.Shelley(
              Arbitrary.arbitrary[AddressBytes].sample.get,
              Value.Ada(
                Coin(
                  transaction.body.outputs.head
                      .asInstanceOf[TransactionOutput.Shelley]
                      .value
                      .asInstanceOf[Value.Ada]
                      .coin
                      .value +
                      transaction.body.fee.value
                      + 1L
                )
              )
            )
          )
        )

        val result = EqualValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
          context,
          state,
          transaction
        )
        assert(result.isLeft)
    }

    test("FeeMutator success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                fee = Arbitrary.arbitrary[Coin].sample.get
              )
            )
        }

        val result = FeeMutator.transit(context, state, transaction)
        assert(result.isRight)
        assert(context.fee == transaction.body.fee)
    }

    test("RemoveInputsFromUtxoMutator success") {
        val context = Context()
        val state = State(
          utxo = genMapOfSizeFromArbitrary[TransactionInput, TransactionOutput](1, 4).sample.get
        )
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = state.utxo.keySet
              )
            )
        }

        val result = RemoveInputsFromUtxoMutator.transit(context, state, transaction)
        assert(state.utxo.nonEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.isEmpty)
    }

    test("AddOutputsToUtxoMutator success") {
        val context = Context()
        val state = State()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                outputs = genListOfSizeFromArbitrary[TransactionOutput](1, 4).sample.get
              )
            )
        }

        val result = AddOutputsToUtxoMutator.transit(context, state, transaction)
        assert(state.utxo.isEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.values.toSeq == transaction.body.outputs)
    }

    test("CardanoMutator success") {
        val context = Context()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[AddressBytes].sample.get,
                    Value.Ada(Coin(Gen.choose(0L, 1000000L).sample.get))
                  )
                ),
                fee = Coin(Gen.choose(0L, 1000000L).sample.get),
                collateralInputs =
                    Some(genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get),
                referenceInputs = None
              )
            )
        }
        val state = State(
          utxo = transaction.body.collateralInputs.get.view
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .concat(
                Seq(
                  transaction.body.inputs.head -> TransactionOutput.Shelley(
                    Arbitrary.arbitrary[AddressBytes].sample.get,
                    Value.Ada(
                      Coin(
                        transaction.body.outputs.head
                            .asInstanceOf[TransactionOutput.Shelley]
                            .value
                            .asInstanceOf[Value.Ada]
                            .coin
                            .value +
                            transaction.body.fee.value
                      )
                    )
                  )
                )
              )
              .toMap
        )

        val result = CardanoMutator.transit(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty)
        assert(transaction.body.referenceInputs.isEmpty)
        assert(transaction.body.inputs.forall(state.utxo.contains))
        assert(transaction.body.collateralInputs.get.forall(state.utxo.contains))
        assert(context.fee == transaction.body.fee)
        assert(state.utxo.nonEmpty)
        assert(!transaction.body.inputs.forall(result.toOption.get.utxo.contains))
        assert(transaction.body.outputs.forall(result.toOption.get.utxo.values.toSeq.contains))
    }

    private def randomValidTransaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)
}
