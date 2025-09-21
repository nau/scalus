package scalus.cardano.ledger.txbuilder

import monocle.Lens
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Script.{Native, PlutusV1, PlutusV2, PlutusV3}
import scalus.cardano.ledger.utils.{MinCoinSizedTransactionOutput, MinTransactionFee, TxBalance}
import scalus.cardano.ledger.ProtocolParams
import monocle.syntax.all.*
import scalus.ledger.api.Timelock

import scala.annotation.tailrec
import scala.collection.immutable.{SortedMap, SortedSet, TreeSet}
import scala.util.Random

case class Environment(
    protocolParams: ProtocolParams,
    evaluator: PlutusScriptEvaluator,
    network: Network,
    era: Era = Era.Conway,
)

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
    tx.focus(_.body.value).modify(f)
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

type DiffHandler = (Long, Transaction) => Transaction

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

            val t = tx
                .focus(_.body.value.outputs.index(changeOutputIdx))
                .replace(newChangeOut)
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
            val txWithExUnits = computeScriptsWitness(resolvedUtxo, evaluator, protocolParams)(tx)

            MinTransactionFee(txWithExUnits, resolvedUtxo, protocolParams) match
                case Right(minFee) =>
                    // don't go below initial fee
                    val fee = Coin(math.max(minFee.value, tx.body.value.fee.value))
                    val txWithFees = setFee(fee)(txWithExUnits)
                    // find the diff
                    val diff = calculateChangeLovelace(txWithFees, resolvedUtxo, protocolParams)
                    // try to balance it
                    diffHandler(diff, txWithFees) match
                        case Right(balanced) =>
                            val balancedDiff =
                                calculateChangeLovelace(balanced, resolvedUtxo, protocolParams)
                            iteration += 1
                            // if diff is zero, we are done
                            if balancedDiff == 0 then Right(balanced)
                            else if iteration > 20 then
                                Left(TxBalancingError.CantBalance(balancedDiff))
                            else loop(tx) // try again
                        case Left(error) => Left(error)
                case Left(error) => Left(TxBalancingError.Failed(error))
        }
        loop(initial)
    }
}

/*
/** Owns a set of UTxOs.
 *
  * Knows how to spend them in a transaction, which
 *   - for pubkey utxos, fills in the witness set with the necessary signatures
 *   - for plutus script utxos, can resolve them by address and run them, and fills in the witness
 *     set with the necessary redeemers and plutus data
 *   - for native script utxos, ...
 */
trait Party {
    def utxos: Map[TransactionInput, ResolvedOutput]
    def approve(tx: Transaction): Transaction
    def diffHandlerFor(input: TransactionInput): DiffHandler
}

enum ResolvedOutput:
    case Pubkey(in: TransactionInput, out: TransactionOutput)
    case PlutusScript(
        in: TransactionInput,
        out: TransactionOutput,
        script: scalus.cardano.ledger.PlutusScript
    )
    case NativeScript(in: TransactionInput, out: TransactionOutput, script: Timelock)

extension (r: ResolvedOutput) {
    def getOutput = r match {
        case ResolvedOutput.Pubkey(in, out)               => out
        case ResolvedOutput.PlutusScript(in, out, script) => out
        case ResolvedOutput.NativeScript(in, out, script) => out
    }
    def getInput = r match {
        case ResolvedOutput.Pubkey(in, out)               => in
        case ResolvedOutput.PlutusScript(in, out, script) => in
        case ResolvedOutput.NativeScript(in, out, script) => in
    }

    def canCover(value: Value) = r.getOutput.value.coin >= value.coin
    def spendsFrom(address: Address) = r.getOutput.address == address
}

trait Intention
case class Pay(from: Address, to: Address, value: Value) extends Intention

case class TxBuilderContext(
    env: Environment,
    parties: Seq[Party]
) {

    def resolveRequirements(intention: Intention): (Party, ResolvedOutput) = intention match {
        case Pay(from, to, value) =>
            val result: (Party, ResolvedOutput) = (for {
                party <- parties
                (in, out) <- party.utxos
                result = (party, out) if out.canCover(value) && out.spendsFrom(from)
            } yield result).headOption.getOrElse(throw NoFittingInputFound(intention))
            result

        case _ => ???
    }

    def realize(intentions: Seq[Intention]): Transaction = {
        // realizing intentions _is_
        val reqs = intentions.map(resolveRequirements)
        val balancer: (Long, Transaction) => Either[TxBalancingError, Transaction] = makeBalancer(
          reqs
        )
        val initial = fillOutTransaction(intentions)
        LowLevelTxBuilder
            .balanceFeeAndChange(
              initial,
              balancer,
              env.protocolParams,
              Map.empty,
              env.evaluator
            )
            .right
            .get
    }

    def makeBalancer(
        requirements: Any
    ): (Long, Transaction) => Either[TxBalancingError, Transaction] = {
        ???
    }

    def fillOutTransaction(intentions: Seq[Intention]): Transaction = ???
}

trait RequirementGatheringError extends Exception
case class NoFittingInputFound(i: Intention) extends RequirementGatheringError
 */
