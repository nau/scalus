package scalus.cardano.ledger.txbuilder

import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

case class DelayedRedeemerSpec(
    utxo: TransactionUnspentOutput,
    redeemerBuilder: Transaction => Data,
    validator: PlutusScript,
    datum: Option[Data]
)

case class PaymentBuilder(
    context: BuilderContext,
    wallet: Wallet,
    payments: Seq[(Address, Value, Option[DatumOption])] = Seq.empty,
    scriptInputs: Set[(TransactionUnspentOutput, Witness)] = Set.empty,
    delayedRedeemers: Seq[DelayedRedeemerSpec] = Seq.empty,
    collateral: Option[(TransactionUnspentOutput, Witness)] = None,
    additionalSteps: Seq[TransactionBuilderStep] = Seq.empty
) {

    def payTo(address: Address, value: Value, datum: Option[DatumOption] = None): PaymentBuilder =
        copy(payments = payments :+ (address, value, datum))

    def payToScript(address: Address, value: Value, datum: Data): PaymentBuilder =
        payTo(address, value, Some(DatumOption.Inline(datum)))

    def collateral(c: (TransactionUnspentOutput, Witness)) = copy(collateral = Some(c))

    def spendScriptOutputs(
        utxo: (TransactionInput, TransactionOutput),
        redeemer: Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): PaymentBuilder = {
        val witness = ThreeArgumentPlutusScriptWitness(
          scriptSource = ScriptSource.PlutusScriptValue(validator),
          redeemer = redeemer,
          datum = datum.map(Datum.DatumValue.apply).getOrElse(Datum.DatumInlined),
          additionalSigners = Set.empty
        )
        copy(
          scriptInputs = scriptInputs + ((TransactionUnspentOutput(utxo), witness))
        )
    }

    def spendUsingDelayedRedeemer(
        utxo: (TransactionInput, TransactionOutput),
        redeemerBuilder: Transaction => Data,
        validator: PlutusScript,
        datum: Option[Data] = None
    ): PaymentBuilder = {
        val spec = DelayedRedeemerSpec(
          utxo = TransactionUnspentOutput(utxo),
          redeemerBuilder = redeemerBuilder,
          validator = validator,
          datum = datum
        )
        copy(delayedRedeemers = delayedRedeemers :+ spec)
    }

    def spendOutputs(
        utxo: (TransactionInput, TransactionOutput),
        witness: Witness
    ) = witness match {
        case w: PubKeyWitness.type =>
            withStep(TransactionBuilderStep.Spend(TransactionUnspentOutput(utxo), w))
        case w: NativeScriptWitness =>
            withStep(TransactionBuilderStep.Spend(TransactionUnspentOutput(utxo), w))
        case w: ThreeArgumentPlutusScriptWitness =>
            withStep(TransactionBuilderStep.Spend(TransactionUnspentOutput(utxo), w))
        case _: TwoArgumentPlutusScriptWitness =>
            ???
    }

    def withStep(step: TransactionBuilderStep): PaymentBuilder =
        copy(additionalSteps = additionalSteps :+ step)

    def withSteps(steps: Seq[TransactionBuilderStep]): PaymentBuilder =
        copy(additionalSteps = additionalSteps ++ steps)

    private def replaceDelayedRedeemers(
        txContext: TransactionBuilder.Context,
        specs: Seq[DelayedRedeemerSpec],
        sortedTx: Transaction
    ): Either[String, TransactionBuilder.Context] = {
        try {
            val updatedRedeemers = specs.foldLeft(txContext.redeemers) { (redeemers, spec) =>
                val realRedeemerData = spec.redeemerBuilder(sortedTx)
                redeemers.map {
                    case dr @ DetachedRedeemer(_, RedeemerPurpose.ForSpend(input))
                        if input == spec.utxo.input =>
                        dr.copy(datum = realRedeemerData)
                    case other => other
                }
            }

            Right(txContext.replaceRedeemers(updatedRedeemers))
        } catch {
            case e: Exception =>
                Left(s"Failed to compute delayed redeemer: ${e.getMessage}")
        }
    }

    def build(): Either[String, Transaction] = {
        val totalRequired = payments.foldLeft(Value.zero)((acc, p) => acc + p._2)

        for {
            walletInputsWithWitnesses <- context.wallet
                .selectInputs(totalRequired)
                .toRight("Insufficient funds in wallet")

            inputSteps = (scriptInputs ++ walletInputsWithWitnesses).map { case (utxo, witness) =>
                witness match {
                    case w: PubKeyWitness.type =>
                        TransactionBuilderStep.Spend(utxo, w)
                    case w: NativeScriptWitness =>
                        TransactionBuilderStep.Spend(utxo, w)
                    case w: ThreeArgumentPlutusScriptWitness =>
                        TransactionBuilderStep.Spend(utxo, w)
                    case _: TwoArgumentPlutusScriptWitness =>
                        ???
                }
            }

            delayedInputSteps = delayedRedeemers.map { d =>
                // a non-unit dummy redeemer data
                val dummyRedeemerData = Data.I(0)
                val witness = ThreeArgumentPlutusScriptWitness(
                  scriptSource = ScriptSource.PlutusScriptValue(d.validator),
                  redeemer = dummyRedeemerData,
                  datum = d.datum.map(Datum.DatumValue.apply).getOrElse(Datum.DatumInlined),
                  additionalSigners = Set.empty
                )
                TransactionBuilderStep.Spend(d.utxo, witness)
            }

            outputSteps = payments.map { case (addr, value, datum) =>
                TransactionBuilderStep.Send(
                  TransactionOutput.Babbage(
                    address = addr,
                    value = value,
                    datumOption = datum,
                    scriptRef = None
                  )
                )
            }

            collateralSteps = collateral
                .map(Seq(_))
                .getOrElse(context.wallet.collateralInputs)
                .map(x => TransactionBuilderStep.AddCollateral(x._1))

            allSteps =
                inputSteps ++ delayedInputSteps ++ outputSteps ++ collateralSteps ++ additionalSteps

            txContext <- TransactionBuilder
                .build(context.env.network, allSteps.toSeq)
                .left
                .map(_.toString)

            resolvedContext <-
                if delayedRedeemers.nonEmpty then {
                    replaceDelayedRedeemers(txContext, delayedRedeemers, txContext.transaction)
                } else {
                    Right(txContext)
                }

            diffHandler = (diff: Long, tx: Transaction) =>
                Change.handleChange(
                  diff,
                  tx,
                  context.wallet.owner,
                  context.env.protocolParams
                )

            finalCtx <- resolvedContext
                .finalizeContext(
                  protocolParams = context.env.protocolParams,
                  diffHandler = diffHandler,
                  evaluator = context.env.evaluator,
                  validators = Seq.empty
                )
                .left
                .map(_.toString)
        } yield finalCtx.transaction
    }

    private def explainBalancingError(error: TxBalancingError): String = error match {
        case TxBalancingError.EvaluationFailed(psee) =>
            s"Plutus script evaluation failed: ${psee.getMessage}, execution trace: ${psee.logs.mkString(" <CR> ")}"
        case TxBalancingError.Failed(cause) =>
            s"Transaction balancing failed: ${cause.getMessage}"
        case TxBalancingError.CantBalance(lastDiff) =>
            s"Cannot balance transaction. Last difference: $lastDiff lovelace"
        case TxBalancingError.InsufficientFunds(diff, required) =>
            s"Insufficient funds. Need $required more lovelace (current difference: $diff)"
    }
}

object PaymentBuilder {
    def apply(context: BuilderContext): PaymentBuilder =
        new PaymentBuilder(context, context.wallet)
}
