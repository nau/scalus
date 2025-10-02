package scalus.cardano.ledger.txbuilder

import monocle.Lens
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{Native, PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.ledger.txbuilder.Intention.Stake
import scalus.cardano.ledger.utils.{MinCoinSizedTransactionOutput, MinTransactionFee, TxBalance}
import scalus.cardano.ledger.ProtocolParams
import monocle.syntax.all.*

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet, TreeSet}
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
    case Mint(value: scalus.cardano.ledger.Mint, scriptInfo: MintIntention, targetAddress: Address)
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
    wallet: Wallet,
    environment: Environment,
    changeReturnStrategy: ChangeReturnStrategy,
    feePayerStrategy: FeePayerStrategy,
    evaluator: PlutusScriptEvaluator
) extends Interpreter {

    override def realize(i: Intention): Transaction = {
        val initialBody = TransactionBody(
          TaggedOrderedSet.empty,
          IndexedSeq.empty,
          Coin.zero,
          collateralInputs = TaggedOrderedSet.from(wallet.collateralInputs.view.map(_.utxo._1))
        )
        val body = i match {
            case Intention.Pay(address, value, data) =>
                val (input, newWallet) = wallet.getInput(value.coin)
                initialBody.copy(
                  inputs = TaggedOrderedSet(input.input),
                  outputs = IndexedSeq(Sized(TransactionOutput(address, value, data)))
                )

            case Intention.Mint(mintValue, scriptInfo, targetAddress) =>
                val tempOutput = TransactionOutput(
                  targetAddress,
                  Value(Coin(1), MultiAsset(mintValue.assets))
                )
                val sizedTempOutput = Sized(tempOutput)
                val minCoin =
                    MinCoinSizedTransactionOutput(sizedTempOutput, environment.protocolParams)
                val correctedOutput = TransactionOutput(
                  targetAddress,
                  Value(minCoin, MultiAsset(mintValue.assets))
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
        val balanced = balancingLoop(
          environment.protocolParams,
          i,
          tx,
          wallet,
          evaluator,
          feePayerStrategy,
          changeReturnStrategy
        )
        balanced
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
        val inputSignees = wallet.inputs.collect { case pubkey: ResolvedTxInput.Pubkey =>
            pubkey.output.address
        }
        val collateralInputSignees = wallet.collateralInputs.collect {
            case pubkey: ResolvedTxInput.Pubkey => pubkey.output.address
        }
        val inputSigneesCount = (inputSignees ++ collateralInputSignees).size
        val requiredSigneesCount = body.requiredSigners.toSeq.size
        val dummyVkeysNeeded = inputSigneesCount + requiredSigneesCount
        addNDummyVKeys(n = dummyVkeysNeeded, ws)
    }

    // Looks up the script-protected inputs and initializes the witness set with redeemers with respective indices.
    private def assembleWsForPayments = {
        wallet.inputs.toSeq
            .sortBy(_.input)
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
                        wallet.inputs
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
                        wallet.inputs
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
    network: Network,
    era: Era = Era.Conway,
)

trait InputSelector {
    def inputs: Set[ResolvedTxInput]
    def collateralInputs: Set[ResolvedTxInput]
}
object InputSelector {
    def apply(
        paymentInputs: Set[ResolvedTxInput],
        collateral: Set[ResolvedTxInput]
    ): InputSelector =
        new InputSelector {
            override def inputs: Set[ResolvedTxInput] = paymentInputs
            override def collateralInputs: Set[ResolvedTxInput] = collateral
        }
}

trait ChangeReturnStrategy {
    def returnChange(
        lovelace: Long,
        body: TransactionBody,
        utxo: UTxO
    ): IndexedSeq[TransactionOutput]

    def minifyChange(
        body: TransactionBody,
        protocolParams: ProtocolParams
    ): IndexedSeq[TransactionOutput]
}
object ChangeReturnStrategy {
    def toAddress(address: Address): ChangeReturnStrategy = new ChangeReturnStrategy {
        override def returnChange(
            lovelace: Long,
            body: TransactionBody,
            utxo: UTxO
        ): IndexedSeq[TransactionOutput] = {
            val outputs = body.outputs.map(_.value)
            if !outputs.exists(_.address == address) then {
                TransactionOutput(address, Value.lovelace(lovelace)) +: outputs
            } else {
                outputs.map {
                    case x if x.address == address => x + Coin(lovelace)
                    case x                         => x
                }
            }
        }

        override def minifyChange(
            body: TransactionBody,
            protocolParams: ProtocolParams
        ): IndexedSeq[TransactionOutput] = {
            val outputs = body.outputs.map(_.value)
            if !outputs.exists(_.address == address) then {
                outputs :+ outputOfAmountOrMin(0, address, protocolParams = protocolParams)
            } else {
                outputs.map {
                    case x if x.address == address =>
                        outputOfAmountOrMin(0, address, protocolParams = protocolParams)
                    case x => x
                }
            }
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

trait Wallet {
    def getInput(targetValue: Coin): (ResolvedTxInput, Wallet)
    def utxo: UTxO
    def inputs: Set[ResolvedTxInput]
    def collateralInputs: Set[ResolvedTxInput]
}
object Wallet {
    def create(paymentInputs: Set[ResolvedTxInput], collat: Set[ResolvedTxInput]): Wallet =
        new Wallet { self =>
            override def getInput(
                targetValue: Coin
            ): (ResolvedTxInput, Wallet) = {
                val resolvedTxInput = paymentInputs.toList
                    .sortBy(resolved => resolved.output.value.coin.value)
                    .collectFirst {
                        case resolved if resolved.output.value.coin >= targetValue => resolved
                    }
                    .get
                (resolvedTxInput, self)
            }

            override def utxo: UTxO =
                (paymentInputs ++ collat).groupBy(_.input).mapValues(x => x.head.output).toMap

            override def inputs: Set[ResolvedTxInput] = paymentInputs

            override def collateralInputs: Set[ResolvedTxInput] = collat
        }
}

def balancingLoop(
    protocolParams: ProtocolParams,
    intention: Intention,
    initial: Transaction,
    wallet: Wallet,
    evaluator: PlutusScriptEvaluator,
    feePayerStrategy: FeePayerStrategy,
    changeReturnStrategy: ChangeReturnStrategy
) = {
    val i @ Intention.Pay(targetAddress, value, datum) = intention

    var iteration = -1
    @tailrec
    def loop(w: Wallet, tx: Transaction): Transaction = {
        iteration += 1
        val a = ensurePaymentOutputExists(i)(tx)
        val b = ensureChangeOutputExists(changeReturnStrategy, w.utxo, protocolParams)(a)
        val c = ceilOuts(protocolParams)(b)
        val d = computeScriptsWitness(w.utxo, evaluator, protocolParams)(c)
        val e = setMinFee(w, protocolParams)(d)
        // 1) Set the current diff as change
        // 2) Calculate the fee, then use it to (re)-calculate the change
        // 3) check again: if we overshot, we just scratch the change, and re-do the thing
        //                 if we undershot, just re-loop, and let the next iteration handle it
        val diff = calculateChangeLovelace(e, w.utxo, protocolParams)
        if diff == 0 then {
            e
        } else if diff > 0 then {
            loop(w, e)
        } else {
            val f = modifyBody(
              e,
              body =>
                  body.copy(outputs =
                      changeReturnStrategy
                          .minifyChange(d.body.value, protocolParams)
                          .map(Sized.apply)
                  )
            )
            loop(w, f)
        }
    }
    loop(wallet, setMinFee(wallet, protocolParams)(initial))
}

def calculateChangeLovelace(tx: Transaction, utxo: UTxO, params: ProtocolParams): Long = {
    val produced = TxBalance.produced(tx)
    val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
    consumed.coin.value - produced.coin.value
}

def calculateChangeValue(tx: Transaction, utxo: UTxO, params: ProtocolParams): Value = {
    val produced = TxBalance.produced(tx)
    val consumed = TxBalance.consumed(tx, CertState.empty, utxo, params).toTry.get
    consumed - produced
}

def setupRedeemers(protocolParams: ProtocolParams, tx: Transaction, redeemers: Seq[Redeemer]) = {
    if redeemers.isEmpty then {
        tx
    } else {
        val rawRedeemers = KeepRaw(Redeemers.from(redeemers))
        val usedModels = ScriptDataHashGenerator.getUsedCostModels(
          protocolParams,
          tx.witnessSet,
          TreeSet.empty[Language]
        )
        val scriptDataHash: Hash[Blake2b_256, HashPurpose.ScriptDataHash] =
            ScriptDataHashGenerator.computeScriptDataHash(
              Era.Conway,
              Some(rawRedeemers),
              tx.witnessSet.plutusData,
              usedModels
            )

        tx.copy(
          body = KeepRaw(tx.body.value.copy(scriptDataHash = Some(scriptDataHash))),
          witnessSet = tx.witnessSet.copy(redeemers = Some(rawRedeemers)),
        )
    }
}
def computeScriptsWitness(
    utxo: UTxO,
    evaluator: PlutusScriptEvaluator,
    protocolParams: ProtocolParams
)(tx: Transaction): Transaction = {
    val redeemers = evaluator.evalPlutusScripts(tx, utxo)
    setupRedeemers(protocolParams, tx, redeemers)
}

def ensureChangeOutputExists(
    changeReturnStrategy: ChangeReturnStrategy,
    utxo: UTxO,
    protocolParams: ProtocolParams
)(tx: Transaction) = {
    val amount = calculateChangeLovelace(tx, utxo, protocolParams)
    val outs =
        if amount < 0 then changeReturnStrategy.minifyChange(tx.body.value, protocolParams)
        else changeReturnStrategy.returnChange(amount, tx.body.value, utxo)
    modifyBody(tx, _.copy(outputs = outs.map(Sized.apply)))
}

def ensurePaymentOutputExists(i: Intention.Pay)(tx: Transaction) = {
    val exists = tx.body.value.outputs.exists(out =>
        out.value.address == i.address && out.value.value == i.value
    )
    if exists then tx
    else
        modifyBody(
          tx,
          _.copy(outputs =
              tx.body.value.outputs :+ Sized(TransactionOutput(i.address, i.value, i.data))
          )
        )

}

def ceilOuts(protocolParams: ProtocolParams)(tx: Transaction): Transaction = {
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

def setFee(amount: Coin)(tx: Transaction) = modifyBody(tx, _.copy(fee = amount))
def setMinFee(wallet: Wallet, protocolParams: ProtocolParams)(tx: Transaction) = {
    val min = MinTransactionFee(tx, wallet.utxo, protocolParams).toTry.get
    setFee(min)(tx)
}

def outputOfAmountOrMin(
    lovelace: Long,
    address: Address,
    datumOption: Option[DatumOption] = None,
    protocolParams: ProtocolParams
) = {
    val output = TransactionOutput(address, Value.lovelace(lovelace), datumOption)
    val minAmount = MinCoinSizedTransactionOutput(Sized(output), protocolParams)
    if lovelace < minAmount.value then {
        output.withValue(Value(minAmount))
    } else {
        output.withValue(Value.lovelace(lovelace))
    }
}

extension (t: TransactionOutput) {
    def valueLens: Lens[TransactionOutput, Value] =
        Lens[TransactionOutput, Value](_.value)(v => txout => txout.withValue(v))

    def withValue(amount: Value): TransactionOutput = t match {
        case shelley: TransactionOutput.Shelley =>
            shelley.copy(value = amount)
        case babbage: TransactionOutput.Babbage =>
            babbage.copy(value = amount)
    }
}
def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody): Transaction = {
    val newBody = f(tx.body.value)
    tx.copy(body = KeepRaw(newBody))
}

/*
class TransactionRequirements {}

object BlahBuilder {

    def mktx(): Unit = {

        case class Account(wallet: Wallet)
        enum FeePayer {
            case Input(idx: Int)
            case Account
            case Address // ??
        }
        enum Change {
            case OutputIndex // simple case, create this output
            case Output // simple case, create this output
            case Account // merge with same account output
        }

        case class InputDesc(
            input: TransactionInput,
            output: TransactionOutput, // Script + Datum + Redeemer + output certain order,
            // Native script: multisig
            // PubkeyHash (produce signature)
        ) {
            def howToSpendOutput(): Unit = ???
        }

        case class PayTo(
            party: Party,
            assets: Value,
            datumOption: Option[DatumOption] = None,
            minAdaPayer: () => Party
        )

        enum Party {
            case Address(address: Address)
            case Script(address: ShelleyPaymentPart.Script, datum: Data)
            case Select(strategy: () => TransactionInput) // side effect
            case Utxo(input: TransactionInput)
            case Wallet(address: Address)
        }

        PayTo(Alice, 100000)
        PayTo(Bob, 200000)
        Pay(from = Alice, to = Bob, 10000)
        Mint(Asset, Bob)
        WithdrawRewards(Alice, 500000, destination = Bob)
        WithdrawRewards(Alice, 500000, destination = Change)

        val inputs = ??? // intentions SpendFrom
        val outputs = ??? // intentions of PayTo
        val changeOutput = ??? // change
        val utxo = resolveInputs(inputs)
        val withdrawals = Map(RewardAccount => Party)
        val certificates = ??? // intentions Stake
        val mint = Map(Asset => OutputDesc)
        while notBalanced do
            ensureOutputsHaveMinAda(outputs, params)
            if changeOutput.lovelace < minAda then {
                val inputs = feePayerWallet.selectUtxosToCover(minAda - changeOutput.lovelace)
                updateInputs(inputs)
            }
            val fee = calculateFee(tx, params)
            balanceTx()
            if scripts then
                val exunits = evaluateScripts(tx, utxo, params)
                // always PubKeyHash
                collateralInputs = selectCollateralInputs(exunits, params) // Collateral Party
                collateralReturn = ???

            balanceTx()
            addSignatures() // add dummy signatures first
            balanceTx()
            // allow for script redeemers to be modified
            // for example to add input indexes to redeemers
//            callbackScripts(tx => tx)
            balanceTx()

        signTx()
        // assert num signatures == num dummy signatures
    }

}
 */
/*
 * ## Levels
 * ### Low Level:
     - Transaction building helpers:
            mkTransactionOutput(address, value, datumOption)
 * ### High Level:
    - Intentions

    Intentions interpreter (Tx3) => TransactionRequirements
    Semigroup[TransactionRequirements]
    TransactionRequirements(params) => Transaction
 */

// Transaction balancing error types
enum TxBalancingError {
    case Failed(cause: Throwable)
    case CantBalance(lastDiff: Long)
    case InsufficientFunds(diff: Long, minRequired: Long)
}

object LowLevelTxBuilder {
    class ChangeOutputDiffHandler(protocolParams: ProtocolParams, changeOutputIdx: Int) {
        def changeOutputDiffHandler(
            diff: Long,
            tx: Transaction
        ): Either[TxBalancingError, Transaction] = {
            val numOutputs = tx.body.value.outputs.size
            require(
              changeOutputIdx < numOutputs,
              s"Change output index $changeOutputIdx is out of bounds for outputs of size $numOutputs"
            )
            val changeOut = tx.body.value.outputs(changeOutputIdx)
            val changeLovelace = changeOut.value.value.coin.value
            val updatedLovelaceChange = changeLovelace + diff
            val newValue = changeOut.value.value
                .focus(_.coin.value)
                .replace(updatedLovelaceChange)
            val newChangeOut = Sized(changeOut.value.withValue(newValue))
            val minAda = MinCoinSizedTransactionOutput(newChangeOut, protocolParams)

            if updatedLovelaceChange < minAda.value then
                return Left(
                  TxBalancingError.InsufficientFunds(diff, minAda.value - updatedLovelaceChange)
                )

            val tb = tx.body.value
                .focus(_.outputs.index(changeOutputIdx))
                .replace(newChangeOut)
            val t = tx.copy(body = KeepRaw(tb))
            Right(t)
        }
    }

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - only ADA is adjusted, native tokens must be balanced beforehand
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        changeOutputIdx: Int,
        protocolParams: ProtocolParams,
        resolvedUtxo: UTxO,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        balanceFeeAndChange(
          initial,
          new ChangeOutputDiffHandler(protocolParams, changeOutputIdx).changeOutputDiffHandler,
          protocolParams,
          resolvedUtxo,
          evaluator
        )
    }

    /** Balances the transaction using a diff handler to adjust the transaction.
      *
      * Invariants:
      *   - only ADA is adjusted, native tokens must be balanced beforehand
      *   - fees never go below the initial fee
      */
    def balanceFeeAndChange(
        initial: Transaction,
        diffHandler: (Long, Transaction) => Either[TxBalancingError, Transaction],
        protocolParams: ProtocolParams,
        resolvedUtxo: UTxO,
        evaluator: PlutusScriptEvaluator,
    ): Either[TxBalancingError, Transaction] = {
        var iteration = 0
        @tailrec def loop(tx: Transaction): Either[TxBalancingError, Transaction] = {
            iteration += 1
            if iteration > 20 then return Left(TxBalancingError.CantBalance(0))
            val providedTxFee = tx.body.value.fee
            val txWithExUnits = computeScriptsWitness(resolvedUtxo, evaluator, protocolParams)(tx)

            MinTransactionFee(txWithExUnits, resolvedUtxo, protocolParams) match
                case Right(minFee) =>
                    // don't go below initial fee
                    val fee = Coin(math.max(minFee.value, initial.body.value.fee.value))
                    val txWithFees = setFee(fee)(txWithExUnits)

                    // if modifying the fee changed the transaction size - re-loop
                    if providedTxFee != fee then
                        MinTransactionFee(txWithFees, resolvedUtxo, protocolParams) match
                            case Right(newMinFee) if newMinFee != minFee =>
                                return loop(txWithFees)
                            case Right(_) =>
                            // Fee is still correct, continue
                            case Left(error) => return Left(TxBalancingError.Failed(error))

                    // find the diff
                    val diff = calculateChangeLovelace(txWithFees, resolvedUtxo, protocolParams)
                    // try to balance it
                    diffHandler(diff, txWithFees) match
                        case Right(balanced) =>
                            val balancedDiff =
                                calculateChangeLovelace(balanced, resolvedUtxo, protocolParams)
                            // if diff is zero, we are done
                            if balancedDiff == 0 then Right(balanced)
                            else loop(balanced) // try again with the balanced tx
                        case Left(error) => Left(error)
                case Left(error) => Left(TxBalancingError.Failed(error))
        }
        loop(initial)
    }

    /*
    
    Low level:
    
    balanceFeeChange(tx, changeOutputIdx, protocolParams, resolvedUtxo, evaluator): Either[NotEnoughMoney | Error, Transaction]
    
    balanceChange(tx, changeOutputIdx, protocolParams, resolvedUtxo, evaluator): Either[NotEnoughMoney | Error, Transaction]
    
    balanceTransaction(tx, changeReturnStrategy, feePayerStrategy, wallet, protocolParams, evaluator): Either[NotEnoughMoney | Error, Transaction]
    
     *
    * */
}
