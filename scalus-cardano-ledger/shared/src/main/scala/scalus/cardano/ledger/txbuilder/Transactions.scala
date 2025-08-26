package scalus.cardano.ledger.txbuilder

import scalus.builtin.Data
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.ledger
import scalus.cardano.ledger.utils.TxBalance
import scalus.ledger.babbage.ProtocolParams
import scalus.builtin.ByteString.given

/*
 * Intention is what the user intends to happen. Should accept as few parameters as possible.
 *
 * To turn intention into a transaction usually involves performing side effects, e.g. querying utxos.
 * These are usually executed either in the interpreter, or when assembling the interpreter.
 *
 * TODO: an unresolved question is: whose responsibility is it to choose the inputs?
 *       On one hand, picking the utxos from a wallet is definitely the job of the interpreter.
 *       On the other hand, "I want to pay X ada using these utxos" is a sound intention, and it mentions inputs, so a respective
 *       program would be `Intention.Pay(x, y, myUtxos)`.
 */
enum Intention {
    case Pay(address: Address, value: Value, data: Option[DatumOption] = None)
    case Mint(
        mintValue: ledger.Mint,
        mintingPolicy: PlutusScript,
        redeemer: Data,
        targetAddress: Address
    )
    case RegisterStake
}

trait Interpreter {
    def realize(intention: Intention): Transaction
}

case class InterpreterWithProvidedData(
    inputSelector: InputSelector,
    utxo: UTxO,
    environment: Environment,
    changeReturnStrategy: ChangeReturnStrategy,
    feePayerStrategy: FeePayerStrategy,
    evaluator: PlutusScriptEvaluator
) extends Interpreter {

    override def realize(intention: Intention): Transaction = intention match {
        case pay: Intention.Pay   => realizePay(pay)
        case mint: Intention.Mint => realizeMint(mint)
        case Intention.RegisterStake =>
            ???
    }

    private def realizePay(i: Intention.Pay) = {
        val inputs = utxo.toSeq.map(_._1)
        val body = TransactionBody(
          TaggedOrderedSet.from(inputSelector.inputs.view.map(_.utxo._1)),
          IndexedSeq(Sized(TransactionOutput(i.address, i.value, i.data))),
          Coin.zero,
          collateralInputs = TaggedOrderedSet.from(inputSelector.collateralInputs)
        )
        val ws: TransactionWitnessSet = assembleWs
        val tx = Transaction(body, ws)
        val redeemers = evaluator.evalPlutusScripts(tx, utxo)
        val postEvalTx = tx.copy(witnessSet =
            tx.witnessSet.copy(redeemers = Some(KeepRaw(Redeemers.from(redeemers))))
        )
        TxBalance.doBalance2(tx)(
          utxo,
          environment.protocolParams,
          changeReturnStrategy,
          feePayerStrategy
        )
    }

    private def realizeMint(i: Intention.Mint) = {
        val body = TransactionBody(
          TaggedOrderedSet.from(inputSelector.inputs.view.map(_.utxo._1)),
          IndexedSeq(
            Sized(
              TransactionOutput(
                i.targetAddress,
                Value(Coin.zero, MultiAsset(i.mintValue.assets)),
                None
              )
            )
          ),
          Coin.zero,
          mint = Some(i.mintValue),
          collateralInputs = TaggedOrderedSet.from(inputSelector.collateralInputs)
        )
        val ws: TransactionWitnessSet = assembleWsForMinting(i)
        val tx = Transaction(body, ws)
        val redeemers = evaluator.evalPlutusScripts(tx, utxo)
        val postEvalTx = tx.copy(witnessSet =
            tx.witnessSet.copy(redeemers = Some(KeepRaw(Redeemers.from(redeemers))))
        )
        TxBalance.doBalance2(postEvalTx)(
          utxo,
          environment.protocolParams,
          changeReturnStrategy,
          feePayerStrategy
        )
    }

    // Looks up the script-protected inputs and initializes the witness set with redeemers with respective indices.
    private def assembleWs = {
        inputSelector.inputs.toSeq
            .sortBy(_.utxo._1)
            .zipWithIndex
            .foldLeft(ScriptsWs()) {
                case (
                      ws,
                      (
                        ResolvedTxInput.Script(
                          _,
                          script @ Script.PlutusV1(bytes),
                          redeemer,
                          data
                        ),
                        index
                      )
                    ) =>
                    ws.addV1(script)
                        .addRedeemer(Redeemer(RedeemerTag.Spend, index, redeemer, ExUnits.zero))
                case (
                      ws,
                      (
                        ResolvedTxInput.Script(
                          _,
                          script @ Script.PlutusV2(bytes),
                          redeemer,
                          data
                        ),
                        index
                      )
                    ) =>
                    ws.addV2(script)
                        .addRedeemer(Redeemer(RedeemerTag.Spend, index, redeemer, ExUnits.zero))
                case (
                      ws,
                      (
                        ResolvedTxInput.Script(
                          _,
                          script @ Script.PlutusV3(bytes),
                          redeemer,
                          data
                        ),
                        index
                      )
                    ) =>
                    ws.addV3(script)
                        .addRedeemer(Redeemer(RedeemerTag.Spend, index, redeemer, ExUnits.zero))
                case (ws, _) => ws
            }
            .toWs
    }

    private def assembleWsForMinting(mintIntention: Intention.Mint) = {
        val baseWs = assembleWs
        val policyId = mintIntention.mintingPolicy.scriptHash
        val mintIndex = mintIntention.mintValue.assets.keySet.toArray.sorted.indexOf(policyId)
        val wsWithMintingScript = mintIntention.mintingPolicy match {
            case Script.PlutusV1(bytes) =>
                ScriptsWs(
                  v1Plutus = baseWs.plutusV1Scripts + Script.PlutusV1(bytes),
                  v2Plutus = baseWs.plutusV2Scripts,
                  v3Plutus = baseWs.plutusV3Scripts,
                  redeemers = Set(
                    Redeemer(RedeemerTag.Mint, mintIndex, mintIntention.redeemer, ExUnits.zero)
                  )
                )
            case Script.PlutusV2(bytes) =>
                ScriptsWs(
                  v1Plutus = baseWs.plutusV1Scripts,
                  v2Plutus = baseWs.plutusV2Scripts + Script.PlutusV2(bytes),
                  v3Plutus = baseWs.plutusV3Scripts,
                  redeemers = Set(
                    Redeemer(RedeemerTag.Mint, mintIndex, mintIntention.redeemer, ExUnits.zero)
                  )
                )
            case Script.PlutusV3(bytes) =>
                ScriptsWs(
                  v1Plutus = baseWs.plutusV1Scripts,
                  v2Plutus = baseWs.plutusV2Scripts,
                  v3Plutus = baseWs.plutusV3Scripts + Script.PlutusV3(bytes),
                  redeemers = Set(
                    Redeemer(RedeemerTag.Mint, mintIndex, mintIntention.redeemer, ExUnits.zero)
                  )
                )
        }
        wsWithMintingScript.toWs
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

trait InputSelector {
    def inputs: Set[ResolvedTxInput]
    def collateralInputs: Set[TransactionInput]
}
object InputSelector {
    def apply(regularInputs: Set[ResolvedTxInput], collateral: Set[TransactionInput]) =
        new InputSelector {
            override def inputs: Set[ResolvedTxInput] = regularInputs
            override def collateralInputs: Set[TransactionInput] = collateral
        }
}

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
