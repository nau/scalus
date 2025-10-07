package scalus.cardano.ledger.txbuilder

import cats.implicits.*
import monocle.syntax.all.*
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

/** High-level transaction builder API for pay transactions.
  *
  * Focuses on building payment transactions with support for Plutus validators.
  */
case class TxBuilder(
    context: BuilderContext,
    steps: Seq[TransactionBuilderStep] = Seq.empty
) {

    /** Add a step to spend a UTxO from a Plutus script address.
      *
      * @param utxo
      *   The UTxO to spend
      * @param redeemer
      *   The redeemer data for the validator
      * @param validator
      *   The Plutus script validator
      * @param datum
      *   Optional datum witness (required if the UTxO has a datum hash)
      * @return
      *   Updated builder
      */
    def collectFrom(
        utxo: TransactionUnspentOutput,
        redeemer: Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): TxBuilder = {
        val witness = OutputWitness.PlutusScriptOutput(
          witness = ScriptWitness.ScriptValue(validator, Set.empty),
          redeemer = redeemer,
          datum = datum.map(DatumWitness.DatumValue.apply)
        )
        copy(steps = steps :+ TransactionBuilderStep.SpendOutput(utxo, Some(witness)))
    }

    /** Add a step to send funds to an address.
      *
      * @param address
      *   The recipient address
      * @param value
      *   The value to send
      * @param datum
      *   Optional datum to attach to the output
      * @return
      *   Updated builder
      */
    def payTo(address: Address, value: Value, datum: Option[DatumOption] = None): TxBuilder = {
        val output = TransactionOutput.Babbage(
          address = address,
          value = value,
          datumOption = datum,
          scriptRef = None
        )
        copy(steps = steps :+ TransactionBuilderStep.Pay(output))
    }

    /** Add a step to send funds to a script address with an inline datum.
      *
      * @param scriptAddress
      *   The script address
      * @param value
      *   The value to lock
      * @param datum
      *   The datum to attach inline
      * @return
      *   Updated builder
      */
    def payToScript(
        scriptAddress: Address,
        value: Value,
        datum: Data
    ): TxBuilder = {
        payTo(scriptAddress, value, Some(DatumOption.Inline(datum)))
    }

    /** Build and complete the transaction with balancing.
      *
      * @return
      *   Either an error or the finalized transaction
      */
    def complete(): Either[String, Transaction] = {
        for {
            txContext <- TransactionBuilder.build(context.env.network, steps).left.map(_.explain)
            // Use wallet to get change output
            changeOutput <- context.wallet.getInput(Coin(0)) match {
                case Some((resolvedInput, _)) =>
                    Right(Sized(resolvedInput.output))
                case None =>
                    Left("No UTxO available for change output")
            }
            // Add change output to the transaction
            txWithChangeOut = modifyBody(
              txContext.transaction,
              body => body.copy(outputs = body.outputs.toSeq :+ changeOutput)
            )
            changeOutputIdx = txWithChangeOut.body.value.outputs.size - 1
            // Balance the transaction
            utxo = txContext.resolvedUtxos.map(u => u.input -> u.output).toMap
            balanced <- LowLevelTxBuilder
                .balanceFeeAndChange(
                  txWithChangeOut,
                  changeOutputIdx,
                  context.env.protocolParams,
                  utxo,
                  context.env.evaluator
                )
                .left
                .map {
                    case TxBalancingError.Failed(cause) => cause.getMessage
                    case TxBalancingError.CantBalance(lastDiff) =>
                        s"Can't balance: last diff $lastDiff"
                    case TxBalancingError.InsufficientFunds(diff, required) =>
                        s"Insufficient funds: need $required more"
                }
        } yield balanced
    }
}

object TxBuilder {
    def apply(context: BuilderContext): TxBuilder = new TxBuilder(context)
}

extension (context: BuilderContext) {
    def newTx: TxBuilder = TxBuilder(context)
}
