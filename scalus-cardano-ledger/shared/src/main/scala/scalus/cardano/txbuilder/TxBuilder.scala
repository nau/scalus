package scalus.cardano.txbuilder

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

case class TxBuilder(
    context: BuilderContext,
    steps: Seq[TransactionBuilderStep] = Seq.empty,
    manualInputs: Seq[(TransactionInput, TransactionOutput)] = Seq.empty
) {

    def collectFrom(
        utxo: (TransactionInput, TransactionOutput),
        redeemer: Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): TxBuilder = {
        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(validator),
          redeemer = redeemer,
          datum = datum.map(Datum.DatumValue.apply).getOrElse(Datum.DatumInlined),
          additionalSigners = Set.empty
        )
        copy(steps =
            steps :+ TransactionBuilderStep.Spend(
              TransactionUnspentOutput(utxo._1, utxo._2),
              witness
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
        copy(steps = steps :+ TransactionBuilderStep.Send(output))
    }

    def payToScript(
        scriptAddress: Address,
        value: Value,
        datum: Data
    ): TxBuilder = {
        payTo(scriptAddress, value, Some(DatumOption.Inline(datum)))
    }

    private def walletInputSteps: Seq[TransactionBuilderStep] =
        manualInputs.map { case (input, output) =>
            TransactionBuilderStep.Spend(
              TransactionUnspentOutput(input, output),
              PubKeyWitness
            )
        }

    def complete(): Either[String, Transaction] = {
        for {
            txContext <- TransactionBuilder
                .build(context.env.network, walletInputSteps ++ steps)
                .left
                .map(_.toString)

            changeOutput = Sized(
              TransactionOutput(
                address = context.wallet.owner,
                value = Value(Coin(0))
              )
            )

            txWithChangeOut = modifyBody(
              txContext.transaction,
              body => body.copy(outputs = body.outputs.toSeq :+ changeOutput)
            )
            changeOutputIdx = txWithChangeOut.body.value.outputs.size - 1

            utxo = txContext.resolvedUtxos.utxos
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
                    case TxBalancingError.EvaluationFailed(psee) =>
                        s"Plutus script evaluation failed: ${psee.getMessage}, execution trace: ${psee.logs.mkString(" <CR> ")}"
                    case TxBalancingError.Failed(cause)         => cause.getMessage
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
