package scalus.cardano.ledger.txbuilder
import scalus.builtin.Data
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.txbuilder.TxBuilder.withInputsFromUtxos
import scalus.cardano.ledger.utils.TxBalance.modifyBody
import scalus.cardano.ledger.{ExUnits, KeepRaw, PlutusScriptEvaluator, Redeemer, RedeemerTag, Redeemers, Script, Sized, TaggedSet, Transaction, TransactionOutput, UTxO, Value}
import scalus.cardano.ledger.utils.{OnSurplus, TxBalance}
import scalus.ledger.babbage.ProtocolParams

case class ScriptTxBuilder(
    utxo: UTxO,
    protocolParams: ProtocolParams,
    network: Network,
    tx: Transaction = TxBuilder.emptyTx,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer,
    collateral: UTxO = Map.empty
) {

    def payToAddress(address: Address, value: Value): ScriptTxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def withScript(script: Script, datum: Data, redeemer: Data, index: Int): ScriptTxBuilder = {
        val plutusData = TaggedSet.from(
          tx.witnessSet.plutusData.value.toIndexedSeq :+ KeepRaw(datum) :+ KeepRaw(redeemer)
        )
        val updatedWitnessSet = script match {
            case plutusV1: Script.PlutusV1 =>
                tx.witnessSet.copy(
                  plutusV1Scripts = tx.witnessSet.plutusV1Scripts + plutusV1,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer)))
                )
            case plutusV2: Script.PlutusV2 =>
                tx.witnessSet.copy(
                  plutusV2Scripts = tx.witnessSet.plutusV2Scripts + plutusV2,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer)))
                )
            case plutusV3: Script.PlutusV3 =>
                tx.witnessSet.copy(
                  plutusV3Scripts = tx.witnessSet.plutusV3Scripts + plutusV3,
                  plutusData = KeepRaw(plutusData),
                  redeemers = Some(KeepRaw(addRedeemer(index, redeemer)))
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
          exUnits = ScriptTxBuilder.dummyExUnits
        )

        tx.witnessSet.redeemers match {
            case Some(existingRedeemers) =>
                val existing = existingRedeemers.value.toIndexedSeq
                val updated = existing.filterNot(r =>
                    r.tag == RedeemerTag.Spend && r.index == index
                ) :+ newRedeemer
                Redeemers.from(updated)
            case None =>
                Redeemers.from(Seq(newRedeemer))
        }
    }

    def doFinalizeScript(scriptEvaluator: PlutusScriptEvaluator): Transaction =
        TxBalance.doBalanceScript(tx, scriptEvaluator, collateral)(utxo, protocolParams, onSurplus)

    def withCollateral(collateralIn: UTxO): ScriptTxBuilder = copy(collateral = collateralIn)
}

object ScriptTxBuilder {
    // will be updated during balancing
    private def dummyExUnits = ExUnits(memory = 0L, steps = 0L)

    def initialize(utxo: UTxO, protocolParams: ProtocolParams, network: Network): ScriptTxBuilder =
        ScriptTxBuilder(
          utxo,
          protocolParams,
          network,
          withInputsFromUtxos(utxo)
        )
}
