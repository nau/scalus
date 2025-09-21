package scalus.cardano.ledger.txbuilder

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
