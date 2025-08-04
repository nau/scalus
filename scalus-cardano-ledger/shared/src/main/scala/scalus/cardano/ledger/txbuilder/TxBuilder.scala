package scalus.cardano.ledger.txbuilder
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.utils.TxBalance.modifyBody
import scalus.cardano.ledger.utils.{MinTransactionFee, OnSurplus, TxBalance}
import scalus.ledger.babbage.ProtocolParams
import scalus.builtin.Data

import scala.annotation.tailrec

case class TxBuilder(
    utxo: UTxO,
    protocolParams: ProtocolParams,
    network: Network,
    tx: Transaction = TxBuilder.emptyTx,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer
) {

    def payToAddress(address: Address, value: Value): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def onSurplus(onSurplus: OnSurplus): TxBuilder = copy(onSurplus = onSurplus)

    def map(f: Transaction => Transaction): TxBuilder = copy(tx = f(tx))

    def signedBy(vk: VKeyWitness): TxBuilder = {
        val wSet = tx.witnessSet.copy(vkeyWitnesses = tx.witnessSet.vkeyWitnesses + vk)
        copy(tx = tx.copy(witnessSet = wSet))
    }

    def withScript(script: Script, datum: Data, redeemer: Data, index: Int): TxBuilder = {
        val updatedWitnessSet = script match {
            case plutusV1: Script.PlutusV1 =>
                tx.witnessSet.copy(
                    plutusV1Scripts = tx.witnessSet.plutusV1Scripts + plutusV1,
                    plutusData = tx.witnessSet.plutusData + datum + redeemer,
                    redeemers = Some(addRedeemer(index, redeemer))
                )
            case plutusV2: Script.PlutusV2 =>
                tx.witnessSet.copy(
                    plutusV2Scripts = tx.witnessSet.plutusV2Scripts + plutusV2,
                    plutusData = tx.witnessSet.plutusData + datum + redeemer,
                    redeemers = Some(addRedeemer(index, redeemer))
                )
            case plutusV3: Script.PlutusV3 =>
                tx.witnessSet.copy(
                    plutusV3Scripts = tx.witnessSet.plutusV3Scripts + plutusV3,
                    plutusData = tx.witnessSet.plutusData + datum + redeemer,
                    redeemers = Some(addRedeemer(index, redeemer))
                )
            case native: Script.Native =>
                tx.witnessSet.copy(
                    nativeScripts = tx.witnessSet.nativeScripts + native
                )
        }
        copy(tx = tx.copy(witnessSet = updatedWitnessSet))
    }

    private def addRedeemer(index: Int, redeemerData: Data): Redeemers = {
        val newRedeemer = Redeemer(
            tag = RedeemerTag.Spend,
            index = index,
            data = redeemerData,
            exUnits = TxBuilder.dummyExUnits 
        )
        
        tx.witnessSet.redeemers match {
            case Some(existingRedeemers) =>
                val existing = existingRedeemers.toIndexedSeq
                val updated = existing.filterNot(r => r.tag == RedeemerTag.Spend && r.index == index) :+ newRedeemer
                Redeemers.from(updated)
            case None =>
                Redeemers.from(Seq(newRedeemer))
        }
    }

    def doFinalize = TxBalance.doBalance(tx)(utxo, protocolParams, onSurplus)

    def doFinalizeScript(scriptEvaluator: PlutusScriptEvaluator) = 
        TxBalance.doBalanceScript(tx, scriptEvaluator)(utxo, protocolParams, onSurplus)
}

object TxBuilder {
    val emptyTx: Transaction = Transaction(
      TransactionBody(Set.empty, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )

    private def withInputsFromUtxos(utxo: UTxO) = Transaction(
      TransactionBody(utxo.keySet, IndexedSeq.empty, Coin.zero),
      TransactionWitnessSet.empty
    )
    
    // will be updated during balancing
    private def dummyExUnits = ExUnits(memory = 0L, steps = 0L) 

    // Fetching these most likely involves effectful computations, `initialize` is an entry point to the pure API.
    def initialize(utxo: UTxO, protocolParams: ProtocolParams, network: Network): TxBuilder =
        TxBuilder(
          utxo,
          protocolParams,
          network,
          withInputsFromUtxos(utxo)
        )

    def modifyWs(
        tx: Transaction,
        f: TransactionWitnessSet => TransactionWitnessSet
    ): Transaction = {
        val newWs = f(tx.witnessSet)
        tx.copy(witnessSet = newWs)
    }
}

trait UtxoProvider {
    def utxos: UTxO
}
object UtxoProvider {
    def givenUtxos(u: UTxO): UtxoProvider = new UtxoProvider {
        override def utxos: UTxO = u
    }
}
