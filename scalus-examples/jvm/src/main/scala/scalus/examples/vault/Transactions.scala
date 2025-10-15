package scalus.examples.vault

import scalus.builtin.Data.toData
import scalus.builtin.Data
import scalus.cardano.address.Address
import scalus.cardano.ledger.{Credential, DatumOption, Transaction, TransactionInput, TransactionOutput, Value}
import scalus.cardano.ledger.Script.PlutusV3
import scalus.cardano.ledger.txbuilder.{BuilderContext, PaymentBuilder}

class Transactions(context: BuilderContext) {
    val wallet = context.wallet
    val script = PlutusV3(VaultContract.script.cborByteString)
    val scriptAddress = Address(context.env.network, Credential.ScriptHash(script.scriptHash))

    def lock(value: Value, owner: Address = wallet.owner): Either[String, Transaction] = {
        val inputsToSpend = wallet.selectInputs(value).get
        val builder = inputsToSpend.foldLeft(PaymentBuilder(context)) {
            case (builder, (utxo, witness)) =>
                builder.spendOutputs((utxo.input, utxo.output), witness)
        }
        val ownerCredentialHash = owner match {
            case addr: scalus.cardano.address.ShelleyAddress =>
                scalus.builtin.ByteString.fromArray(addr.payment.asHash.bytes)
            case _ =>
                return Left("Only Shelley addresses are supported")
        }
        val datum = Vault.Datum(ownerCredentialHash, Vault.State.Idle, BigInt(value.coin.value))
        builder.payToScript(scriptAddress, value, datum.toData).build()
    }

    def withdraw(
        vaultUtxo: (TransactionInput, TransactionOutput)
    ): Either[String, Transaction] = {
        val currentDatum = vaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[Vault.Datum]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        val newDatum = currentDatum.copy(state = Vault.State.Pending)
        val vaultValue = vaultUtxo._2.value

        val redeemer = Vault.Redeemer.Withdraw.toData
        PaymentBuilder(context)
            .spendScriptOutputs(
              vaultUtxo,
              redeemer,
              script
            )
            .payToScript(scriptAddress, vaultValue, newDatum.toData)
            .build()
    }

    def deposit(
        vaultUtxo: (TransactionInput, TransactionOutput),
        additionalValue: Value
    ): Either[String, Transaction] = {
        val currentDatum = vaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[Vault.Datum]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        val currentValue = vaultUtxo._2.value
        val newValue = currentValue + additionalValue
        val newAmount = BigInt(newValue.coin.value)

        val newDatum = currentDatum.copy(amount = newAmount)

        val redeemer = Vault.Redeemer.Deposit.toData
        PaymentBuilder(context)
            .spendScriptOutputs(vaultUtxo, redeemer, script)
            .payToScript(scriptAddress, newValue, newDatum.toData)
            .build()
    }

    def finalize(
        vaultUtxo: (TransactionInput, TransactionOutput),
        ownerAddress: Address
    ): Either[String, Transaction] = {
        val currentDatum = vaultUtxo._2 match {
            case TransactionOutput.Babbage(_, _, Some(DatumOption.Inline(d)), _) =>
                d.to[Vault.Datum]
            case _ =>
                return Left("Vault UTxO must have an inline datum")
        }

        val vaultValue = vaultUtxo._2.value

        val redeemer = Vault.Redeemer.Finalize.toData
        PaymentBuilder(context)
            .spendScriptOutputs(vaultUtxo, redeemer, script)
            .payTo(ownerAddress, vaultValue)
            .build()
    }

}
