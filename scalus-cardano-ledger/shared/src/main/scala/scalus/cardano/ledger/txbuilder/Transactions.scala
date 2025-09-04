package scalus.cardano.ledger.txbuilder

import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{Native, PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.ledger.txbuilder.Intention.Stake
import scalus.cardano.ledger.utils.TxBalance.modifyBody
import scalus.cardano.ledger.utils.{MinCoinSizedTransactionOutput, TxBalance}
import scalus.ledger.babbage.ProtocolParams

import scala.collection.immutable.SortedMap
import scala.util.Random

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
    case Mint(value: ledger.Mint, scriptInfo: MintIntention, targetAddress: Address)
    case WithdrawRewards(withdrawals: SortedMap[RewardAccount, Coin])

    case Stake(credential: Credential, poolKeyHash: PoolKeyHash)
}

enum MintIntention {
    case UsingNative(nativeScript: Native)
    case UsingPlutus(plutusScript: PlutusScript, redeemer: Data)
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

    override def realize(i: Intention): Transaction = {
        val initialBody = TransactionBody(
          inputs = TaggedOrderedSet.from(inputSelector.inputs.view.map(_.utxo._1)),
          outputs = IndexedSeq.empty,
          fee = Coin.zero,
          collateralInputs =
              TaggedOrderedSet.from(inputSelector.collateralInputs.view.map(_.utxo._1))
        )
        val body = i match {
            case Intention.Pay(address, value, data) =>
                initialBody.copy(
                  outputs = IndexedSeq(Sized(TransactionOutput(address, value, data)))
                )
            case Intention.Mint(mintValue, scriptInfo, targetAddress) =>
                val tempOutput = TransactionOutput(
                  targetAddress,
                  Value(Coin(1), MultiAsset(mintValue.assets)),
                  None
                )
                val sizedTempOutput = Sized(tempOutput)
                val minCoin =
                    MinCoinSizedTransactionOutput(sizedTempOutput, environment.protocolParams)
                val correctedOutput = TransactionOutput(
                  targetAddress,
                  Value(minCoin, MultiAsset(mintValue.assets)),
                  None
                )
                initialBody.copy(
                  outputs = IndexedSeq(Sized(correctedOutput)),
                  mint = Some(mintValue),
                )
            case Intention.WithdrawRewards(withdrawals) =>
                initialBody.copy(
                  withdrawals = Some(Withdrawals(withdrawals))
                )
            case Intention.Stake(credential, poolKeyHash) =>
                val stakeRegDelegCert = Certificate.StakeRegDelegCert(
                  credential = credential,
                  poolKeyHash = poolKeyHash,
                  coin = Coin(environment.protocolParams.stakeAddressDeposit)
                )
                initialBody.copy(
                  certificates = TaggedSet(stakeRegDelegCert)
                )
        }
        val ws = makeWs(i, body)
        val tx = Transaction(body, ws)
        val redeemers = evaluator.evalPlutusScripts(tx, utxo)
        val postEvalTx =
            tx.copy(witnessSet = tx.witnessSet.copy(redeemers = if redeemers.isEmpty then {
                None
            } else Some(KeepRaw(Redeemers.from(redeemers)))))
        val withMinsCeiled = ceilOuts(postEvalTx, environment.protocolParams)
        TxBalance.doBalance2(withMinsCeiled)(
          utxo,
          environment.protocolParams,
          changeReturnStrategy,
          feePayerStrategy
        )

    }

    private def makeWs(intention: Intention, body: TransactionBody) = {
        val ws = intention match {
            case Intention.Pay(address, value, data) =>
                assembleWsForPayments
            case mint: Intention.Mint =>
                val spendingWs = assembleWsForPayments
                val mintingWs = assembleWsForMinting(mint)
                combineWitnessSets(spendingWs, mintingWs)
            case withdraw: Intention.WithdrawRewards =>
                assembleWsForWithdrawal(withdraw)
            case stake: Intention.Stake =>
                assembleWsForStaking(stake, body)
        }
        val inputSignees = inputSelector.inputs.collect { case pubkey: ResolvedTxInput.Pubkey =>
            pubkey.output.address
        }
        val collateralInputSignees = inputSelector.collateralInputs.collect {
            case pubkey: ResolvedTxInput.Pubkey => pubkey.output.address
        }
        val inputSigneesCount = (inputSignees ++ collateralInputSignees).size
        val requiredSigneesCount = body.requiredSigners.toSeq.size
        val dummyVkeysNeeded = inputSigneesCount + requiredSigneesCount
        addNDummyVKeys(n = dummyVkeysNeeded, ws)
    }

    // Looks up the script-protected inputs and initializes the witness set with redeemers with respective indices.
    private def assembleWsForPayments = {
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
        mintIntention.scriptInfo match {
            case MintIntention.UsingNative(nativeScript) =>
                ScriptsWs().addNative(nativeScript).toWs
            case MintIntention.UsingPlutus(plutusScript, redeemer) =>
                val mintingPolicyId = plutusScript.scriptHash
                mintIntention.value.assets.keySet.view.zipWithIndex
                    .collectFirst {
                        case (policyId, index) if policyId == mintingPolicyId =>
                            plutusScript match {
                                case v1: PlutusV1 =>
                                    ScriptsWs()
                                        .addV1(v1)
                                        .addRedeemer(
                                          Redeemer(RedeemerTag.Mint, index, redeemer, ExUnits.zero)
                                        )
                                case v2: PlutusV2 =>
                                    ScriptsWs()
                                        .addV2(v2)
                                        .addRedeemer(
                                          Redeemer(RedeemerTag.Mint, index, redeemer, ExUnits.zero)
                                        )
                                case v3: PlutusV3 =>
                                    ScriptsWs()
                                        .addV3(v3)
                                        .addRedeemer(
                                          Redeemer(RedeemerTag.Mint, index, redeemer, ExUnits.zero)
                                        )
                            }
                    }
                    .getOrElse(ScriptsWs())
                    .toWs
        }

    }

    private def assembleWsForWithdrawal(withdrawIntention: Intention.WithdrawRewards) = {
        val sortedRewardAccounts = withdrawIntention.withdrawals.keySet.toArray.sorted
        sortedRewardAccounts.view.zipWithIndex
            .foldLeft(ScriptsWs()) { case (ws, (rewardAccount, index)) =>
                rewardAccount.scriptHashOption match {
                    case Some(scriptHash) =>
                        inputSelector.inputs
                            .collectFirst {
                                case ResolvedTxInput.Script(_, script, redeemer, _)
                                    if script.scriptHash == scriptHash =>
                                    script match {
                                        case v1: PlutusV1 =>
                                            ws.addV1(v1)
                                                .addRedeemer(
                                                  Redeemer(
                                                    RedeemerTag.Reward,
                                                    index,
                                                    redeemer,
                                                    ExUnits.zero
                                                  )
                                                )
                                        case v2: PlutusV2 =>
                                            ws.addV2(v2)
                                                .addRedeemer(
                                                  Redeemer(
                                                    RedeemerTag.Reward,
                                                    index,
                                                    redeemer,
                                                    ExUnits.zero
                                                  )
                                                )
                                        case v3: PlutusV3 =>
                                            ws.addV3(v3)
                                                .addRedeemer(
                                                  Redeemer(
                                                    RedeemerTag.Reward,
                                                    index,
                                                    redeemer,
                                                    ExUnits.zero
                                                  )
                                                )
                                    }
                            }
                            .getOrElse(ws)
                    case None => ws
                }
            }
            .toWs
    }

    private def assembleWsForStaking(stake: Intention.Stake, body: TransactionBody) = {
        val certificates = body.certificates.toIndexedSeq // .sorted when certs are sorted

        certificates.view.zipWithIndex
            .foldLeft(ScriptsWs()) { case (ws, (certificate, index)) =>
                certificate.scriptHashOption match {
                    case Some(scriptHash) =>
                        inputSelector.inputs
                            .collectFirst {
                                case ResolvedTxInput.Script(_, script, redeemer, _)
                                    if script.scriptHash == scriptHash =>
                                    script match {
                                        case v1: PlutusV1 =>
                                            ws.addV1(v1)
                                                .addRedeemer(
                                                  Redeemer(
                                                    RedeemerTag.Cert,
                                                    index,
                                                    redeemer,
                                                    ExUnits.zero
                                                  )
                                                )
                                        case v2: PlutusV2 =>
                                            ws.addV2(v2)
                                                .addRedeemer(
                                                  Redeemer(
                                                    RedeemerTag.Cert,
                                                    index,
                                                    redeemer,
                                                    ExUnits.zero
                                                  )
                                                )
                                        case v3: PlutusV3 =>
                                            ws.addV3(v3)
                                                .addRedeemer(
                                                  Redeemer(
                                                    RedeemerTag.Cert,
                                                    index,
                                                    redeemer,
                                                    ExUnits.zero
                                                  )
                                                )
                                    }
                            }
                            .getOrElse(ws)
                    case None => ws
                }
            }
            .toWs
    }

    private def addNDummyVKeys(n: Int, ws: TransactionWitnessSet): TransactionWitnessSet = {
        def mkDummyWitness = {
            val key = Random().alphanumeric.take(32).mkString
            val signature = Random().alphanumeric.take(64).mkString
            VKeyWitness(ByteString.fromString(key), ByteString.fromString(signature))
        }
        ws.copy(vkeyWitnesses = Set.fill(n)(mkDummyWitness))
    }

    def ceilOuts(tx: Transaction, protocolParams: ProtocolParams): Transaction = {
        def ceilOut(sizedOut: Sized[TransactionOutput]): Sized[TransactionOutput] = {
            val out = sizedOut.value
            val min = MinCoinSizedTransactionOutput(sizedOut, protocolParams)
            if out.value.coin < min then {
                out match {
                    case shelley @ TransactionOutput.Shelley(_, value, _) =>
                        Sized(shelley.copy(value = value.copy(coin = min)))
                    case babbage @ TransactionOutput.Babbage(_, value, _, _) =>
                        Sized(babbage.copy(value = value.copy(coin = min)))
                }
            } else Sized(out)
        }

        modifyBody(tx, b => b.copy(outputs = b.outputs.map(ceilOut)))
    }

    private def combineWitnessSets(
        ws1: TransactionWitnessSet,
        ws2: TransactionWitnessSet
    ): TransactionWitnessSet = {
        TransactionWitnessSet(
          vkeyWitnesses = ws1.vkeyWitnesses ++ ws2.vkeyWitnesses,
          plutusV1Scripts = ws1.plutusV1Scripts ++ ws2.plutusV1Scripts,
          plutusV2Scripts = ws1.plutusV2Scripts ++ ws2.plutusV2Scripts,
          plutusV3Scripts = ws1.plutusV3Scripts ++ ws2.plutusV3Scripts,
          plutusData = KeepRaw(
            TaggedSet.from(ws1.plutusData.value.toIndexedSeq ++ ws2.plutusData.value.toIndexedSeq)
          ),
          redeemers = (ws1.redeemers, ws2.redeemers) match {
              case (Some(red1), Some(red2)) =>
                  Some(KeepRaw(Redeemers.from(red1.value.toSeq ++ red2.value.toSeq)))
              case (Some(red1), None) => Some(red1)
              case (None, Some(red2)) => Some(red2)
              case (None, None)       => None
          },
          nativeScripts = ws1.nativeScripts ++ ws2.nativeScripts
        )
    }

}

case class ScriptsWs(
    native: Set[Native] = Set.empty,
    v1Plutus: Set[PlutusV1] = Set.empty,
    v2Plutus: Set[PlutusV2] = Set.empty,
    v3Plutus: Set[PlutusV3] = Set.empty,
    redeemers: Set[Redeemer] = Set.empty
) {
    def toWs: TransactionWitnessSet = TransactionWitnessSet(
      nativeScripts = native,
      plutusV1Scripts = v1Plutus,
      plutusV2Scripts = v2Plutus,
      plutusV3Scripts = v3Plutus,
      redeemers = if redeemers.isEmpty then { None }
      else Some(KeepRaw(Redeemers.from(redeemers)))
    )

    def addNative(n: Native): ScriptsWs = copy(native = native + n)
    def addV1(v1: PlutusV1): ScriptsWs = copy(v1Plutus = v1Plutus + v1)
    def addV2(v2: PlutusV2): ScriptsWs = copy(v2Plutus = v2Plutus + v2)
    def addV3(v3: PlutusV3): ScriptsWs = copy(v3Plutus = v3Plutus + v3)
    def addRedeemer(redeemer: Redeemer): ScriptsWs = copy(redeemers = redeemers + redeemer)

}

trait TxSigner {
    def signTx(unsigned: Transaction): Transaction
}
object TxSigner {
    def usingKeyPairs(keyPairs: (ByteString, ByteString)*): TxSigner = new TxSigner {

        // public -> private
        private val keys = Map(keyPairs*)

        override def signTx(unsigned: Transaction): Transaction = {
            val signatures = keys.map { (publicKey, privateKey) =>
                val txHash = unsigned.id
                val signature = platform.signEd25519(privateKey, txHash)
                VKeyWitness(publicKey, signature)
            }.toSet

            // override the dummies
            val ws = unsigned.witnessSet.copy(vkeyWitnesses = signatures)
            unsigned.copy(witnessSet = ws)
        }
    }
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
    case Pubkey(utxo: (TransactionInput, TransactionOutput), data: Option[DatumOption] = None)
    case Script(
        utxo: (TransactionInput, TransactionOutput),
        script: PlutusScript,
        redeemer: Data,
        data: Option[DatumOption] = None
    )
}
extension (r: ResolvedTxInput) {
    def utxo: (TransactionInput, TransactionOutput) = r match {
        case ResolvedTxInput.Pubkey(utxo, _)       => utxo
        case ResolvedTxInput.Script(utxo, _, _, _) => utxo
    }

    def input: TransactionInput = r match {
        case ResolvedTxInput.Pubkey(utxo, data)                   => utxo._1
        case ResolvedTxInput.Script(utxo, script, redeemer, data) => utxo._1
    }

    def output: TransactionOutput = r match {
        case ResolvedTxInput.Pubkey(utxo, data)                   => utxo._2
        case ResolvedTxInput.Script(utxo, script, redeemer, data) => utxo._2
    }
}

case class Environment(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network
)

trait InputSelector {
    def inputs: Set[ResolvedTxInput]
    def collateralInputs: Set[ResolvedTxInput]
}
object InputSelector {
    def apply(
        regularInputs: Set[ResolvedTxInput],
        collateral: Set[ResolvedTxInput]
    ): InputSelector =
        new InputSelector {
            override def inputs: Set[ResolvedTxInput] = regularInputs
            override def collateralInputs: Set[ResolvedTxInput] = collateral
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
