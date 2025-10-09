package scalus.cardano.ledger.txbuilder

import cats.implicits.*
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

case class TxBuilder(
    context: BuilderContext,
    steps: Seq[TransactionBuilderStep] = Seq.empty
) {

    def collectFrom(
        utxo: (TransactionInput, TransactionOutput),
        redeemer: Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): TxBuilder = {
        val witness = OutputWitness.PlutusScriptOutput(
          witness = ScriptWitness.ScriptValue(validator, Set.empty),
          redeemer = redeemer,
          datum = datum.map(DatumWitness.DatumValue.apply)
        )
        copy(steps =
            steps :+ TransactionBuilderStep.SpendOutput(
              TransactionUnspentOutput(utxo._1, utxo._2),
              Some(witness)
            )
        )
    }

    def payTo(address: Address, value: Value, datum: Option[DatumOption] = None): TxBuilder = {
        val output = TransactionOutput.Babbage(
          address = address,
          value = value,
          datumOption = datum,
          scriptRef = None
        )
        copy(steps = steps :+ TransactionBuilderStep.Pay(output))
    }

    def payToScript(
        scriptAddress: Address,
        value: Value,
        datum: Data
    ): TxBuilder = {
        payTo(scriptAddress, value, Some(DatumOption.Inline(datum)))
    }

    private def walletInputSteps: Seq[TransactionBuilderStep] =
        context.wallet.inputs.toSeq.map { input =>
            TransactionBuilderStep.SpendOutput(
              TransactionUnspentOutput(input.input, input.output),
              None
            )
        }

    def complete(): Either[String, Transaction] = {
        for {
            txContext <- TransactionBuilder
                .build(context.env.network, walletInputSteps ++ steps)
                .left
                .map(_.explain)

            changeOutput = Sized(
              TransactionOutput(
                address = context.wallet.changeAddress,
                value = Value(Coin(0))
              )
            )

            txWithChangeOut = modifyBody(
              txContext.transaction,
              body => body.copy(outputs = body.outputs.toSeq :+ changeOutput)
            )
            changeOutputIdx = txWithChangeOut.body.value.outputs.size - 1

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
