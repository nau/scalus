package scalus.cardano.ledger.txbuilder
package wip

import monocle.Focus.focus
import scalus.builtin.ByteString
import scalus.cardano.address.Address
import scalus.cardano.ledger.*

import scala.util.Random

type DiffHandler = (Long, Transaction) => Either[TxBalancingError, Transaction]

/** Owns a set of UTxOs.
  *
  * Knows how to spend them in a transaction, which
  *   - for pubkey utxos, fills in the witness set with the necessary signatures
  *   - for plutus script utxos, can resolve them by address and run them, and fills in the witness
  *     set with the necessary redeemers and plutus data
  *   - for native script utxos, ...
  */
trait Party {
    def utxos: Map[TransactionInput, ResolvedUtxo]
    def approve(tx: Transaction): Transaction
    def diffHandlerFor(input: TransactionInput): DiffHandler
    def getCollateralUtxo: ResolvedUtxo

    def getUtxo(min: Coin): ResolvedUtxo
}

enum ResolvedUtxo:
    case Pubkey(in: TransactionInput, out: TransactionOutput)
    case PlutusScript(
        in: TransactionInput,
        out: TransactionOutput,
        script: scalus.cardano.ledger.PlutusScript
    )
    case NativeScript(in: TransactionInput, out: TransactionOutput, script: Timelock)

extension (r: ResolvedUtxo) {
    def getOutput = r match {
        case ResolvedUtxo.Pubkey(in, out)               => out
        case ResolvedUtxo.PlutusScript(in, out, script) => out
        case ResolvedUtxo.NativeScript(in, out, script) => out
    }
    def getInput = r match {
        case ResolvedUtxo.Pubkey(in, out)               => in
        case ResolvedUtxo.PlutusScript(in, out, script) => in
        case ResolvedUtxo.NativeScript(in, out, script) => in
    }

    def canCover(value: Value) = r.getOutput.value.coin >= value.coin
    def spendsFrom(address: Address) = r.getOutput.address == address
}

case class TxBuilderContext(
    env: Environment,
    parties: Seq[Party]
)

trait Intention {
    type Requirements

    def resolveRequirements(
        context: TxBuilderContext
    ): Either[RequirementGatheringError, Requirements]

    def contributeToTransaction(r: Requirements): TransactionContribution

    def contributeToTransaction(
        context: TxBuilderContext
    ): Either[RequirementGatheringError, TransactionContribution] =
        resolveRequirements(context).map(contributeToTransaction)
}

case class TransactionContribution(
    inputs: Set[TransactionInput] = Set.empty,
    outputs: IndexedSeq[TransactionOutput] = IndexedSeq.empty,
    witnesses: TransactionWitnessSet = TransactionWitnessSet(),
    collateralInputs: Set[TransactionInput] = Set.empty,
    collateralReturn: Option[TransactionOutput] = None,
    fee: Coin = Coin.zero
) {
    def merge(contribution: TransactionContribution): TransactionContribution = {
        /*
         * Alice -> Bob mergeWith Alice -> Charlie
         *
         * AliceInput
         */
        ???
    }

    def toTransaction = Transaction(
      TransactionBody(
        inputs = TaggedOrderedSet.from(inputs),
        outputs = outputs.map(Sized.apply),
        fee = fee,
        collateralInputs = TaggedOrderedSet.from(collateralInputs),
        collateralReturnOutput = collateralReturn.map(Sized.apply)
      ),
      witnesses
    )
}

case class Pay(from: Address, to: Address, value: Value) extends Intention {
    override type Requirements = (Party, ResolvedUtxo)

    override def resolveRequirements(
        context: TxBuilderContext
    ): Either[RequirementGatheringError, (Party, ResolvedUtxo)] = (for {
        party <- context.parties
        (in, out) <- party.utxos
        result = (party, out) if out.canCover(value) && out.spendsFrom(from)
    } yield result).headOption.toRight(NoFittingInputFound(this))

    override def contributeToTransaction(r: (Party, ResolvedUtxo)): TransactionContribution = {
        val (party, utxo) = r
        val output = TransactionOutput(to, value)
        val witnesses = utxo match {
            case ResolvedUtxo.Pubkey(in, out) =>
                TransactionWitnessSet(Set(mkDummyWitness))
            case _ => TransactionWitnessSet()
        }

        utxo match {
            case ResolvedUtxo.Pubkey(in, out) =>
                TransactionContribution(
                  inputs = Set(utxo.getInput),
                  outputs = IndexedSeq(output),
                  witnesses = witnesses
                )
            case ResolvedUtxo.PlutusScript(in, out, script) =>
                val collateralUtxo = party.getCollateralUtxo
                val collateralReturn = TransactionOutput(from, Value.zero)
                TransactionContribution(
                  inputs = Set(utxo.getInput),
                  outputs = IndexedSeq(output),
                  witnesses = witnesses,
                  collateralInputs = Set(collateralUtxo.getInput),
                  collateralReturn = Some(collateralReturn)
                )
            case ResolvedUtxo.NativeScript(in, out, script) =>
                TransactionContribution(
                  inputs = Set(utxo.getInput),
                  outputs = IndexedSeq(output),
                  witnesses = witnesses
                )
        }
    }
}

case class IntentionInterpreter(context: TxBuilderContext) {
    def interpret(intentions: Seq[Intention]) = {
        val contribution = intentions
            .flatMap(_.contributeToTransaction(context).toOption)
            .reduce(_.merge(_))
        // turn it into a tx, pass it to the low lever builder.
        ???
    }

    def interpretOne(intention: Intention) = {
        val contribution = intention.contributeToTransaction(context)
        val tx = contribution.map(_.toTransaction).right.get
        tx
    }

}

trait RequirementGatheringError extends Exception
case class NoFittingInputFound(i: Intention) extends RequirementGatheringError

def mkDummyWitness = {
    val key = Random().alphanumeric.take(32).mkString
    val signature = Random().alphanumeric.take(64).mkString
    VKeyWitness(ByteString.fromString(key), ByteString.fromString(signature))
}
