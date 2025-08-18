package scalus.cardano.ledger.txbuilder
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{AddrKeyHash, Coin, DataHash, DatumOption, ExUnits, KeepRaw, Mint, PlutusScript, Redeemer, RedeemerTag, Redeemers, Script, ScriptHash, Sized, Slot, TaggedSet, Transaction, TransactionBody, TransactionInput, TransactionOutput, TransactionWitnessSet, UTxO, VKeyWitness, Value}
import scalus.cardano.ledger.txbuilder.TxBuilder.{dummyVkey, modifyBody, modifyWs}
import scalus.cardano.ledger.utils.TxBalance

import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

case class TxBuilder(
    context: BuilderContext,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer,
    tx: Transaction = TxBuilder.emptyTx
) {

    def onSurplus(onSurplus: OnSurplus) = copy(onSurplus = onSurplus)

    def mint(address: Address, value: Value, script: Script): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx =
            modifyBody(tx, b => b.copy(mint = Some(Mint(value.assets)), outputs = b.outputs :+ out))
        )
    }

    def payTo(address: Address, value: Value): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def payToScript(address: Address, value: Value, datum: Data): TxBuilder = {
        val out = Sized(TransactionOutput(address, value, Some(DatumOption.Inline(datum))))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def payToScript(address: Address, value: Value, datumHash: DataHash): TxBuilder = {
        val out = Sized(TransactionOutput(address, value, Some(DatumOption.Hash(datumHash))))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def validFrom(slot: Slot): TxBuilder = {
        copy(tx = modifyBody(tx, b => b.copy(validityStartSlot = Some(slot.slot))))
    }

    def withRequiredSigners(signers: Set[AddrKeyHash]): TxBuilder = {
        copy(tx = modifyBody(tx, b => b.copy(requiredSigners = b.requiredSigners ++ signers)))
    }

    def attachSpendingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Spend)
    def attachMintingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Mint)
    def attachCertScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Cert)
    def attachRewardScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Reward)
    def attachVotingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Voting)
    def attachProposingScript(script: PlutusScript, datum: Data, redeemer: Data, index: Int) =
        attachPlutusScript(script, datum, redeemer, index, RedeemerTag.Proposing)

    def attachPlutusScript(
        script: PlutusScript,
        datum: Data,
        redeemer: Data,
        index: Int,
        tag: RedeemerTag
    ) = {
        val plutusData = TaggedSet.from(
          tx.witnessSet.plutusData.value.toIndexedSeq :+ KeepRaw(datum) :+ KeepRaw(redeemer)
        )
        val updatedWitnessSet = script match {
            case plutusV1: Script.PlutusV1 =>
                tx.witnessSet.copy(
                  plutusV1Scripts = tx.witnessSet.plutusV1Scripts + plutusV1,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer, tag)))
                )
            case plutusV2: Script.PlutusV2 =>
                tx.witnessSet.copy(
                  plutusV2Scripts = tx.witnessSet.plutusV2Scripts + plutusV2,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer, tag)))
                )
            case plutusV3: Script.PlutusV3 =>
                tx.witnessSet.copy(
                  plutusV3Scripts = tx.witnessSet.plutusV3Scripts + plutusV3,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer, tag)))
                )
        }
        copy(tx = tx.copy(witnessSet = updatedWitnessSet))
    }

    def attachNativeScript(script: Script.Native, index: Int): TxBuilder = {
        val updatedWitnessSet = tx.witnessSet.copy(
          nativeScripts = tx.witnessSet.nativeScripts + script
        )
        copy(tx = tx.copy(witnessSet = updatedWitnessSet))
    }

    private def addRedeemer(index: Int, redeemerData: Data, tag: RedeemerTag): Redeemers = {
        val newRedeemer = Redeemer(
          tag = tag,
          index = index,
          data = redeemerData,
          exUnits = TxBuilder.dummyExUnits
        )

        tx.witnessSet.redeemers match {
            case Some(existingRedeemers) =>
                val existing = existingRedeemers.value.toIndexedSeq
                val updated =
                    existing.filterNot(r => r.tag == tag && r.index == index) :+ newRedeemer
                Redeemers.from(updated)
            case None =>
                Redeemers.from(Seq(newRedeemer))
        }
    }
    /*

    def spendScriptInput(input: TransactionInput): TxBuilder = {
        val inputs = tx.body.value.inputs + input
        copy(tx = modifyBody(tx, _.copy(inputs = inputs)))
    }

    val intentions: mutable.HashSet[Intention] = ???
    def attach() = ??? //Intention.AttachNativeScript(script: Script.Native)
     */

    def withInputs(inputs: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(inputs = inputs)))
    /*
    enum ScriptInput {
        case Native(script: Script.Native)
        case Plutus(script: PlutusScript, datum: Option[Data], redeemer: Data)
    }

    // what we want a transaction to do
    enum Intention {
        case From(input: TransactionInput)
        case PayTo(address: Address, value: Value)
        case PayToScript(address: Address, value: Value, datum: Option[Data])
        case Mint(address: Address, value: Value, script: Script)
        case AttachScript(script: ScriptInput)
        case AttachNativeScript(script: Script.Native)
        case ValidFrom(slot: Slot)
        case WithRequiredSigners(signers: Set[AddrKeyHash])
    }
    // what we need to know to fulfill the intensions
    enum Requirement {
        case InputNeeded(input: TransactionInput)
        case ScriptNeeded(script: ScriptHash)
        case DatumNeeded(datum: DataHash)
        case TxOutputNeeded(input: TransactionInput)
    }
    // all the info we need to build a transaction
    class ResolvedTxRequirements {
        val intentions: Set[Intention]
        val requirements: Set[Requirement]
        val inputs: SortedSet[TransactionInput]
        val outputs: ArrayBuffer[TransactionOutput]
        val feePayer: Option[TransactionInput]
        val changeOutputs: ArrayBuffer[TransactionOutput]
        val collateralInputs: Set[TransactionInput]
        val mint: Option[Mint]
        val validityStartSlot: Option[Slot]
        val requiredSigners: SortedSet[AddrKeyHash]
        val plutusData: SortedSet[Data]
        val redeemers: Redeemers
        val scripts: SortedSet[Script]
    }

    type Reqs = Set[Requirement]

    def prepare(intentions: Set[Intention]): Reqs

    // resolve intentions and requirements to a context
    trait Resolver {
        def resolve(intentions: Set[Intention], reqs: Reqs): ResolvedTxRequirements = ???
    }
    // pure tx building function
    def build(ctx: ResolvedTxRequirements): Transaction


    case class ResolvedScriptInput(input: TransactionInput, script: ScriptInput, output: TransactionOutput)

    def fromScriptInput(in: ResolvedScriptInput) = ???*/

    def selectInputs(selectInputs: SelectInputs): TxBuilder = withInputs(selectInputs(context.utxo))

    def withCollateral(collateralIn: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(collateralInputs = collateralIn)))

    private def addDummyVkey(tx: Transaction) =
        modifyWs(tx, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses + dummyVkey))

    private def removeDummyVkey(tx: Transaction) =
        modifyWs(tx, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses - dummyVkey))

    def build: Transaction = {
        val withDummyVkey = addDummyVkey(tx)
        val balanced = if isPlutusScriptTx(withDummyVkey) then {
            TxBalance.doBalancePlutusScript(withDummyVkey, context.evaluator)(
              context.utxoProvider.utxo,
              context.protocolParams,
              onSurplus
            )
        } else
            TxBalance.doBalance(withDummyVkey)(
              context.utxoProvider.utxo,
              context.protocolParams,
              onSurplus
            )

        val signed = signTx(removeDummyVkey(balanced))
        context.validate(signed).toTry.get
    }

    private def signTx(txToSign: Transaction): Transaction = {
        if context.signingKeys.isEmpty then {
            txToSign
        } else {
            val signatures = context.signingKeys.map { (publicKey, privateKey) =>
                val signature = platform.signEd25519(privateKey, txToSign.id)
                VKeyWitness(publicKey, signature)
            }.toSet

            modifyWs(txToSign, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses ++ signatures))
        }
    }

    private def isPlutusScriptTx(transaction: Transaction): Boolean =
        transaction.witnessSet.plutusV1Scripts.size +
            transaction.witnessSet.plutusV2Scripts.size +
            transaction.witnessSet.plutusV3Scripts.size > 0
}

object TxBuilder {
    // will be updated during balancing
    private val dummyExUnits = ExUnits(memory = 0L, steps = 0L)

    // needed during fee calculation and prior to signing
    private val dummyVkey = VKeyWitness(
      ByteString.fromString("0".repeat(32)),
      ByteString.fromString("0".repeat(64))
    )

    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody) = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
    }

    def modifyWs(
        tx: Transaction,
        f: TransactionWitnessSet => TransactionWitnessSet
    ): Transaction = {
        val newWs = f(tx.witnessSet)
        tx.copy(witnessSet = newWs)
    }

    val emptyTx: Transaction = Transaction(
      TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )

    def withInputsFromUtxos(utxo: UTxO) = Transaction(
      TransactionBody(utxo.keySet, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )
}
