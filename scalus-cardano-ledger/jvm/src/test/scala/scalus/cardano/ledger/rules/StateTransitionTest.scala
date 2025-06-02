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

    test("not empty spent inputs validation rule success") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
              )
            )
        }

        val result = EmptinessLedgerValidator.Inputs.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty)
    }

    test("not empty spent inputs validation rule failure") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set.empty
              )
            )
        }

        val result = EmptinessLedgerValidator.Inputs.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.inputs.isEmpty)
    }

    test("spent inputs and reference inputs have empty intersection rule success") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get,
                referenceInputs = None
              )
            )
        }

        val result =
            DisjointLedgerValidator.InputsAndReferenceInputs.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty && transaction.body.referenceInputs.isEmpty)
    }

    test("spent inputs and reference inputs have empty intersection rule failure") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
        val transaction = {
            val inputs = genSetOfSizeFromArbitrary[TransactionInput](1, 4).sample.get
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(inputs = inputs, referenceInputs = Some(inputs))
            )
        }

        val result =
            DisjointLedgerValidator.InputsAndReferenceInputs.validate(context, state, transaction)
        assert(result.isLeft)
        assert(transaction.body.inputs.nonEmpty && transaction.body.referenceInputs.nonEmpty)
        assert(transaction.body.inputs == transaction.body.referenceInputs.get)
    }

    test("spent collateral reference inputs must be in utxo rule success") {
        val context: Ledger.Context = ()
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
        val state: Ledger.State.Default = Ledger.State.Default(
          utxo = transaction.body.inputs.view
              .concat(transaction.body.collateralInputs.get)
              .concat(transaction.body.referenceInputs.get)
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .toMap
        )

        val result = MustBeInUtxoLedgerValidator.AllInputs.validate(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.forall(state.utxo.contains(_)))
        assert(transaction.body.collateralInputs.get.forall(state.utxo.contains(_)))
        assert(transaction.body.referenceInputs.get.forall(state.utxo.contains(_)))
    }

    test("spent collateral reference inputs must be in utxo rule failure") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
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

        val result = MustBeInUtxoLedgerValidator.AllInputs.validate(context, state, transaction)
        assert(result.isLeft)
        assert(!transaction.body.inputs.forall(state.utxo.contains(_)))
        assert(!transaction.body.collateralInputs.get.forall(state.utxo.contains(_)))
        assert(!transaction.body.referenceInputs.get.forall(state.utxo.contains(_)))
    }

    test("inputs == outputs + fee success") {
        val context: Ledger.Context = ()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[Address].sample.get,
                    Value.Ada(Coin(Gen.choose(0L, 1000000L).sample.get))
                  )
                ),
                fee = Coin(Gen.choose(0L, 1000000L).sample.get)
              )
            )
        }
        val state: Ledger.State.Default = Ledger.State.Default(
          utxo = Map(
            transaction.body.inputs.head -> TransactionOutput.Shelley(
              Arbitrary.arbitrary[Address].sample.get,
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

        val result = EqualLedgerValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
          context,
          state,
          transaction
        )
        assert(result.isRight)
    }

    test("inputs == outputs + fee failure") {
        val context: Ledger.Context = ()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[Address].sample.get,
                    Value.Ada(Coin(Gen.choose(0L, 1000000L).sample.get))
                  )
                ),
                fee = Coin(Gen.choose(0L, 1000000L).sample.get)
              )
            )
        }
        val state: Ledger.State.Default = Ledger.State.Default(
          utxo = Map(
            transaction.body.inputs.head -> TransactionOutput.Shelley(
              Arbitrary.arbitrary[Address].sample.get,
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

        val result = EqualLedgerValidator.InputsAmountEqualsSumOfOutputsAmountAndFeeAmount.validate(
          context,
          state,
          transaction
        )
        assert(result.isLeft)
    }

    test("FeeLedgerMutator success") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                fee = Arbitrary.arbitrary[Coin].sample.get
              )
            )
        }

        val result = FeeLedgerMutator.transit(context, state, transaction)
        assert(state.fee == Coin.zero)
        assert(result.isRight)
        assert(result.toOption.get.fee == transaction.body.fee)
    }

    test("RemoveInputsFromUtxoLedgerMutator success") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default(
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

        val result = RemoveInputsFromUtxoLedgerMutator.transit(context, state, transaction)
        assert(state.utxo.nonEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.isEmpty)
    }

    test("AddOutputsToUtxoLedgerMutator success") {
        val context: Ledger.Context = ()
        val state: Ledger.State.Default = Ledger.State.Default()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                outputs = genListOfSizeFromArbitrary[TransactionOutput](1, 4).sample.get
              )
            )
        }

        val result = AddOutputsToUtxoLedgerMutator.transit(context, state, transaction)
        assert(state.utxo.isEmpty)
        assert(result.isRight)
        assert(result.toOption.get.utxo.values.toSeq == transaction.body.outputs)
    }

    test("CardanoLedgerMutator success") {
        val context: Ledger.Context = ()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[Address].sample.get,
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
        val state: Ledger.State.Default = Ledger.State.Default(
          utxo = transaction.body.collateralInputs.get.view
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .concat(
                Seq(
                  transaction.body.inputs.head -> TransactionOutput.Shelley(
                    Arbitrary.arbitrary[Address].sample.get,
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

        val result = CardanoLedgerMutator.transit(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty)
        assert(transaction.body.referenceInputs.isEmpty)
        assert(transaction.body.inputs.forall(state.utxo.contains(_)))
        assert(transaction.body.collateralInputs.get.forall(state.utxo.contains(_)))
        assert(state.fee == Coin.zero)
        assert(result.toOption.get.fee == transaction.body.fee)
        assert(state.utxo.nonEmpty)
        assert(!transaction.body.inputs.forall(result.toOption.get.utxo.contains(_)))
        assert(transaction.body.outputs.forall(result.toOption.get.utxo.values.toSeq.contains(_)))
    }

    test("HydrozoaLedgerMutator success") {
        val context: Ledger.Context = ()
        val transaction = {
            val tx = randomValidTransaction
            tx.copy(
              body = tx.body.copy(
                inputs = Set(
                  Arbitrary.arbitrary[TransactionInput].sample.get
                ),
                outputs = List(
                  TransactionOutput.Shelley(
                    Arbitrary.arbitrary[Address].sample.get,
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
        val state: Ledger.State.Default = Ledger.State.Default(
          utxo = transaction.body.collateralInputs.get.view
              .map(_ -> Arbitrary.arbitrary[TransactionOutput].sample.get)
              .concat(
                Seq(
                  transaction.body.inputs.head -> TransactionOutput.Shelley(
                    Arbitrary.arbitrary[Address].sample.get,
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

        val result = hydrozoa.HydrozoaLedgerMutator.transit(context, state, transaction)
        assert(result.isRight)
        assert(transaction.body.inputs.nonEmpty)
        assert(transaction.body.referenceInputs.isEmpty)
        assert(transaction.body.inputs.forall(state.utxo.contains(_)))
        assert(transaction.body.collateralInputs.get.forall(state.utxo.contains(_)))
        assert(state.fee == Coin.zero)
        assert(result.toOption.get.fee == Coin.zero)
        assert(state.utxo.nonEmpty)
        assert(!transaction.body.inputs.forall(result.toOption.get.utxo.contains(_)))
        assert(transaction.body.outputs.forall(result.toOption.get.utxo.values.toSeq.contains(_)))
    }

    private def randomValidTransaction =
        Arbitrary.arbitrary[Transaction].sample.get.copy(isValid = true)
}
