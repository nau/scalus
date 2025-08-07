package scalus.cardano.ledger.txbuilder
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.TxBuilder.modifyBody
import scalus.cardano.ledger.utils.TxBalance

case class TxBuilder(
                        txContext: BuilderContext,
                        tx: Transaction = TxBuilder.emptyTx,
) {

    def payToAddress(address: Address, value: Value): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def withScript(script: Script, datum: Data, redeemer: Data, index: Int): TxBuilder = {
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

    def signedBy(vk: VKeyWitness): TxBuilder = {
        val wSet = tx.witnessSet.copy(vkeyWitnesses = tx.witnessSet.vkeyWitnesses + vk)
        copy(tx = tx.copy(witnessSet = wSet))
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
                val existing = existingRedeemers.value.toIndexedSeq
                val updated = existing.filterNot(r =>
                    r.tag == RedeemerTag.Spend && r.index == index
                ) :+ newRedeemer
                Redeemers.from(updated)
            case None =>
                Redeemers.from(Seq(newRedeemer))
        }
    }

    def mapWs(f: TransactionWitnessSet => TransactionWitnessSet): Transaction = {
        val newWs = f(tx.witnessSet)
        tx.copy(witnessSet = newWs)
    }

    def withCollateral(collateralIn: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(collateralInputs = collateralIn)))

    def doFinalize(scriptEvaluator: PlutusScriptEvaluator): Transaction =
        if isScriptTx then {
            TxBalance.doBalanceScript(tx, scriptEvaluator)(
              txContext.utxoProvider.utxo,
              txContext.protocolParams,
              txContext.onSurplus
            )
        } else
            TxBalance.doBalance(tx)(
              txContext.utxoProvider.utxo,
              txContext.protocolParams,
              txContext.onSurplus
            )

    private def isScriptTx: Boolean =
        (tx.witnessSet.nativeScripts ++ tx.witnessSet.plutusV1Scripts ++ tx.witnessSet.plutusV2Scripts ++ tx.witnessSet.plutusV3Scripts).nonEmpty
}

object TxBuilder {
    // will be updated during balancing
    private def dummyExUnits = ExUnits(memory = 0L, steps = 0L)

    def modifyBody(tx: Transaction, f: TransactionBody => TransactionBody) = {
        val newBody = f(tx.body.value)
        tx.copy(body = KeepRaw(newBody))
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
