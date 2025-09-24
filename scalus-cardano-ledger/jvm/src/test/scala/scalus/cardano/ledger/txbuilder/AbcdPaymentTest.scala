package scalus.cardano.ledger.txbuilder
package wip

import scalus.cardano.address.Address
import scalus.cardano.ledger.{TransactionOutput, Value}

class AbcdPaymentTest {}

class AbcdPaymentIntention(
    a: Address,
    b: Address,
    abValue: Value,
    c: Address,
    d: Address,
    cdValue: Value
) extends Intention {

    override type Requirements = AbcdRequirements

    override def resolveRequirements(
        context: TxBuilderContext
    ): Either[RequirementGatheringError, AbcdRequirements] = ???

    override def contributeToTransaction(r: AbcdRequirements): TransactionContribution = {
        if a != c && b != d then twoPayersTwoReceivers(r)
        else if a == c && b != d then onePayerTwoReceivers(r)
        else if a != c && b == d then twoPayersOneReceiver(r)
        else throw IllegalStateException()

    }

    private def makeTxOutput(v: Value, to: Address) = TransactionOutput(to, v)

    private def twoPayersTwoReceivers(r: AbcdRequirements): TransactionContribution =
        val aUtxo = r.a.getUtxo(abValue.coin)
        val cUtxo = r.c.getUtxo(cdValue.coin)
        TransactionContribution(
          Set(aUtxo.getInput, cUtxo.getInput),
          IndexedSeq(makeTxOutput(abValue, b), makeTxOutput(cdValue, d))
        )

    private def onePayerTwoReceivers(r: AbcdRequirements) =
        val utxo = r.a.getUtxo(abValue.coin + cdValue.coin)
        TransactionContribution(
          Set(utxo.getInput),
          IndexedSeq(makeTxOutput(abValue, b), makeTxOutput(cdValue, d))
        )

    private def twoPayersOneReceiver(r: AbcdRequirements) =
        val aUtxo = r.a.getUtxo(abValue.coin)
        val cUtxo = r.c.getUtxo(cdValue.coin)
        TransactionContribution(
          Set(aUtxo.getInput, cUtxo.getInput),
          IndexedSeq(makeTxOutput(abValue + cdValue, d))
        )
}

case class AbcdRequirements(a: Party, c: Party)
