package scalus.cardano.ledger.txbuilder

import scalus.builtin.Data
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.ledger.utils.TxBalance
import scalus.ledger.babbage.ProtocolParams

/*
 * Intentions are what I want to happen -- should ask as little as possible, i.e. not take too many parameters.
 *
 * Assemblers (currently only pay assembler) (naming tbd), assemble everything that's needed to realize an intention.
 * `assemble` is the most sideffect heavy function, running the scripts, doing the utxo querying, etc.
 *
 * The builder itself is not doing much, which is probably a sign that it's either unnecessary, or should
 * actually do something (maybe run scripts?).
 *
 * The whole thing is WIP, and the entry point should be calling a function, not creating entities via constructors.
 *
 * There probably needs to be a new entity, which is going to hide Intentions and Assemblers, and also orchestrate the chaining,
 * take parameters 1 by 1, etc.
 */

enum Intention {
    case Pay(address: Address, value: Value, data: Option[DatumOption] = None)
    case Mint
    case RegisterStake
}

def assemble(p: Intention.Pay)(
    environmentGetter: EnvironmentGetter,
    collateral: Set[TransactionInput],
    inputs: Set[ResolvedTxInput],
    resolver: TransactionResolver,
    evaluator: PlutusScriptEvaluator
): PayAssembler = PayAssembler(
  p,
  environmentGetter,
  collateral,
  inputs,
  resolver,
  evaluator
)

case class PayAssembler(
    intention: Intention.Pay,
    environmentGetter: EnvironmentGetter,
    collateral: Set[TransactionInput],
    inputs: Set[ResolvedTxInput],
    resolver: TransactionResolver,
    evaluator: PlutusScriptEvaluator,
) {

    def addPubkeyInput(in: TransactionInput, data: Option[DatumOption]) =
        copy(inputs = inputs + resolver.resolvePubkey(in, data))

    def addScriptInput(
        in: TransactionInput,
        script: PlutusScript,
        redeemer: Data,
        data: Option[DatumOption]
    ) = copy(inputs = inputs + resolver.resolveScript(in, script, redeemer, data))

    def assemble(): PayTxBuilder = {
        val utxo = Map(inputs.toSeq.map(_.utxo)*)
        val body = TransactionBody(
          inputs.map(_.utxo._1),
          IndexedSeq(Sized(TransactionOutput(intention.address, intention.value, intention.data))),
          Coin.zero,
          collateralInputs = collateral
        )
        val ws = inputs.toSeq
            .sortBy(_.utxo._1)
            .zipWithIndex
            .foldLeft(ScriptsWs()) {
                case (
                      ws,
                      (
                        ResolvedTxInput.Script(_, script @ Script.PlutusV1(bytes), redeemer, data),
                        index
                      )
                    ) =>
                    ws.addV1(script)
                        .addRedeemer(Redeemer(RedeemerTag.Spend, index, redeemer, ExUnits.zero))
                case (
                      ws,
                      (
                        ResolvedTxInput.Script(_, script @ Script.PlutusV2(bytes), redeemer, data),
                        index
                      )
                    ) =>
                    ws.addV2(script)
                        .addRedeemer(Redeemer(RedeemerTag.Spend, index, redeemer, ExUnits.zero))
                case (
                      ws,
                      (
                        ResolvedTxInput.Script(_, script @ Script.PlutusV3(bytes), redeemer, data),
                        index
                      )
                    ) =>
                    ws.addV3(script)
                        .addRedeemer(Redeemer(RedeemerTag.Spend, index, redeemer, ExUnits.zero))
                case (ws, _) => ws
            }
            .toWs
        val tx = Transaction(body, ws)
        val redeemers = evaluator.evalPlutusScripts(tx, utxo)
        val postEvalTx = tx.copy(witnessSet =
            tx.witnessSet.copy(redeemers = Some(KeepRaw(Redeemers.from(redeemers))))
        )
        PayTxBuilder(utxo, postEvalTx, environmentGetter.get)
    }
}

case class PayTxBuilder(
    utxo: UTxO,
    initialTx: Transaction,
    environment: Environment
) {
    def build(feePayerStrategy: FeePayerStrategy, changeReturnStrategy: ChangeReturnStrategy) = {
        TxBalance.doBalance2(initialTx)(
          utxo,
          environment.protocolParams,
          changeReturnStrategy,
          feePayerStrategy
        )
    }
}

case class ScriptsWs(
    v1Plutus: Set[PlutusV1] = Set.empty,
    v2Plutus: Set[PlutusV2] = Set.empty,
    v3Plutus: Set[PlutusV3] = Set.empty,
    redeemers: Set[Redeemer] = Set.empty
) {
    def toWs = TransactionWitnessSet(
      plutusV1Scripts = v1Plutus,
      plutusV2Scripts = v2Plutus,
      plutusV3Scripts = v3Plutus,
      redeemers = Some(KeepRaw(Redeemers.from(redeemers)))
    )

    def addV1(v1: PlutusV1) = copy(v1Plutus = v1Plutus + v1)
    def addV2(v2: PlutusV2) = copy(v2Plutus = v2Plutus + v2)
    def addV3(v3: PlutusV3) = copy(v3Plutus = v3Plutus + v3)
    def addRedeemer(redeemer: Redeemer) = copy(redeemers = redeemers + redeemer)

}

trait TransactionResolver {
    def resolveScript(
        input: TransactionInput,
        script: PlutusScript,
        redeemer: Data,
        data: Option[DatumOption]
    ): ResolvedTxInput.Script
    def resolvePubkey(input: TransactionInput, data: Option[DatumOption]): ResolvedTxInput.Pubkey
}

trait EnvironmentGetter {
    def get: Environment
}
object EnvironmentGetter {
    def apply(e: Environment): EnvironmentGetter = new EnvironmentGetter {
        override def get: Environment = e
    }
}

enum ResolvedTxInput {
    case Pubkey(utxo: (TransactionInput, TransactionOutput), data: Option[DatumOption])
    case Script(
        utxo: (TransactionInput, TransactionOutput),
        script: PlutusScript,
        redeemer: Data,
        data: Option[DatumOption] = None
    )
}
extension (r: ResolvedTxInput) {
    def utxo = r match {
        case ResolvedTxInput.Pubkey(utxo, _)       => utxo
        case ResolvedTxInput.Script(utxo, _, _, _) => utxo
    }
}

case class Environment(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network
)

trait ChangeReturnStrategy {
    def returnChange(
        changeAmount: Coin,
        body: TransactionBody,
        utxo: UTxO
    ): IndexedSeq[TransactionOutput]
}

object ChangeReturnStrategy {
    def toAddress(address: Address): ChangeReturnStrategy =
        (changeAmount: Coin, body: TransactionBody, utxo: UTxO) =>
            val outputs = body.outputs.map(_.value)
            if !outputs.exists(_.address == address) then {
                TransactionOutput(address, Value(changeAmount)) +: outputs
            } else {
                outputs.map {
                    case x if x.address == address => x + changeAmount
                    case x                         => x
                }
            }
}

trait FeePayerStrategy {
    def apply(
        feeAmount: Coin,
        outputs: IndexedSeq[TransactionOutput]
    ): IndexedSeq[TransactionOutput]
}

object FeePayerStrategy {
    def subtractFromFirstOutput: FeePayerStrategy =
        (feeAmount: Coin, outputs: IndexedSeq[TransactionOutput]) =>
            outputs match {
                case head +: rest => (head - feeAmount) +: rest
                // unsure if we have to `case _ =>`, because a transaction with no outputs will fail down the line anyway
            }
    def subtractFromAddress(address: Address): FeePayerStrategy =
        (feeAmount: Coin, outputs: IndexedSeq[TransactionOutput]) =>
            outputs.map {
                case o if o.address == address => o - feeAmount
                case o                         => o
            }
}

extension (output: TransactionOutput) {
    def -(coin: Coin): TransactionOutput = {
        val newValue = output.value - Value(coin)
        output match {
            case shelley: TransactionOutput.Shelley => shelley.copy(value = newValue)
            case babbage: TransactionOutput.Babbage => babbage.copy(value = newValue)
        }
    }

    def +(coin: Coin): TransactionOutput = {
        val newValue = output.value + Value(coin)
        output match {
            case shelley: TransactionOutput.Shelley => shelley.copy(value = newValue)
            case babbage: TransactionOutput.Babbage => babbage.copy(value = newValue)
        }
    }
}
