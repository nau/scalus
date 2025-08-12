package scalus.cardano.ledger.txbuilder
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Coin, DataHash, DatumOption, ExUnits, KeepRaw, Mint, Redeemer, RedeemerTag, Redeemers, Script, Sized, TaggedSet, Transaction, TransactionBody, TransactionInput, TransactionOutput, TransactionWitnessSet, UTxO, VKeyWitness, Value}
import scalus.cardano.ledger.txbuilder.TxBuilder.{dummyVkey, modifyBody, modifyWs}
import scalus.cardano.ledger.utils.TxBalance

case class TxBuilder(context: BuilderContext, tx: Transaction = TxBuilder.emptyTx) {

    def payAndMint(address: Address, value: Value): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx =
            modifyBody(tx, b => b.copy(mint = Some(Mint(value.assets)), outputs = b.outputs :+ out))
        )
    }

    def payToAddress(address: Address, value: Value): TxBuilder = {
        val out = Sized(TransactionOutput(address, value))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def payToAddress(address: Address, value: Value, datum: Data) = {
        val out = Sized(TransactionOutput(address, value, Some(DatumOption.Inline(datum))))
        copy(tx = modifyBody(tx, b => b.copy(outputs = b.outputs :+ out)))
    }

    def payToAddress(address: Address, value: Value, datumHash: DataHash) = {
        val out = Sized(TransactionOutput(address, value, Some(DatumOption.Hash(datumHash))))
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

    def withScript(script: Script.Native, index: Int): TxBuilder = {
        val updatedWitnessSet = tx.witnessSet.copy(
          nativeScripts = tx.witnessSet.nativeScripts + script
        )
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
                val existing = existingRedeemers.value.toIndexedSeq
                val updated = existing.filterNot(r =>
                    r.tag == RedeemerTag.Spend && r.index == index
                ) :+ newRedeemer
                Redeemers.from(updated)
            case None =>
                Redeemers.from(Seq(newRedeemer))
        }
    }

    def withInputs(inputs: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(inputs = inputs)))

    def selectInputs(selectInputs: SelectInputs): TxBuilder = withInputs(selectInputs(context.utxo))

    def withCollateral(collateralIn: Set[TransactionInput]): TxBuilder =
        copy(tx = modifyBody(tx, _.copy(collateralInputs = collateralIn)))

    private def addDummyVkey(tx: Transaction) =
        modifyWs(tx, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses + dummyVkey))

    private def removeDummyVkey(tx: Transaction) =
        modifyWs(tx, ws => ws.copy(vkeyWitnesses = ws.vkeyWitnesses - dummyVkey))

    def doFinalize: Transaction = {
        val withDummyVkey = addDummyVkey(tx)
        val balanced = if isScriptTx(withDummyVkey) then {
            TxBalance.doBalanceScript(withDummyVkey, context.evaluator)(
              context.utxoProvider.utxo,
              context.protocolParams,
              context.onSurplus
            )
        } else
            TxBalance.doBalance(withDummyVkey)(
              context.utxoProvider.utxo,
              context.protocolParams,
              context.onSurplus
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

    private def isScriptTx(transaction: Transaction): Boolean =
        (transaction.witnessSet.nativeScripts ++
            transaction.witnessSet.plutusV1Scripts ++
            transaction.witnessSet.plutusV2Scripts ++
            transaction.witnessSet.plutusV3Scripts).nonEmpty
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
