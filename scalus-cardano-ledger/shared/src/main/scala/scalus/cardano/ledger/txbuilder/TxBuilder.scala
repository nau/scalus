package scalus.cardano.ledger.txbuilder
import scalus.builtin.Data
import scalus.cardano.address.{Address, Network}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.txbuilder.TxBuilder.modifyBody
import scalus.cardano.ledger.utils.TxBalance
import scalus.ledger.babbage.ProtocolParams

case class TxBuilder(
    utxo: UTxO,
    protocolParams: ProtocolParams,
    network: Network,
    tx: Transaction = TxBuilder.emptyTx,
    onSurplus: OnSurplus = OnSurplus.toFirstPayer,
    collateral: UTxO = Map.empty
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

    def withCollateral(collateralIn: UTxO): TxBuilder = copy(collateral = collateralIn)

    def onSurplus(onSurplus: OnSurplus): TxBuilder = copy(onSurplus = onSurplus)

    def doFinalize(scriptEvaluator: PlutusScriptEvaluator): Transaction = {
        if isScriptTx then {
            TxBalance.doBalanceScript(tx, scriptEvaluator, collateral)(
              utxo,
              protocolParams,
              onSurplus
            )
        } else TxBalance.doBalance(tx)(utxo, protocolParams, onSurplus)
    }

    def isScriptTx =
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

    // Fetching these most likely involves effectful computations, `initialize` is an entry point to the pure API.
    def initialize(utxo: UTxO, protocolParams: ProtocolParams, network: Network): TxBuilder =
        TxBuilder(
          utxo,
          protocolParams,
          network,
          withInputsFromUtxos(utxo)
        )
}

trait OnSurplus {
    def apply(utxo: UTxO, surplus: Coin): Transaction => Transaction
}
object OnSurplus {
    import TxBuilder.modifyBody
    def toFee: OnSurplus = (_, surplus: Coin) =>
        tx => {
            val fee = tx.body.value.fee + surplus
            modifyBody(tx, _.copy(fee = fee))
        }

    def toAddress(address: Address): OnSurplus = (_, surplus: Coin) =>
        tx => {
            val changeOutput = TransactionOutput(address, Value(surplus))
            modifyBody(tx, b => b.copy(outputs = b.outputs :+ Sized(changeOutput)))

        }

    def toFirstPayer: OnSurplus = (utxo: UTxO, surplus: Coin) =>
        tx => {
            val firstPayer = utxo.head
            toAddress(firstPayer._2.address)(utxo, surplus)(tx)
        }
}
