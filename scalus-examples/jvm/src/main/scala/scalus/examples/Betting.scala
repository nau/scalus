package scalus.examples

import scalus.*
import scalus.ledger.api.v3.*
import scalus.ledger.api.v1.Address
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.ledger.api.v2.OutputDatum
import scalus.builtin.Data
import scalus.builtin.Data.{FromData, ToData}
import scalus.Compiler.compile
import scalus.cardano.plutus.contract.blueprint.{Application, Blueprint}

// https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/blob/main/bet/onchain/aiken/validators/bet.ak

/** Represents the state of a two-player betting game The bet starts with player1 creating it, then
  * player2 can join The oracle decides the winner and triggers the payout
  */
case class BetDatum(
    /** The public key hash of the first player (bet creator) */
    player1: PubKeyHash,
    /** The public key hash of the second player (None if no one has joined yet) */
    player2: PubKeyHash,
    /** The public key hash of the trusted oracle who will announce the winner */
    oracle: PubKeyHash,
    /** The expiration time of the bet (in seconds since the epoch) */
    expiration: PosixTime
) derives FromData,
      ToData

/** Actions that can be performed on the betting contract */
enum Action derives FromData, ToData:
    /** Action for player2 to join an existing bet */
    case JOIN

    /** Action for the oracle to announce the winner and trigger payout */
    case ANNOUNCE_WINNER(winner: PubKeyHash)

/** Main betting validator */
@Compile
object Betting extends Validator:
    override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        ()

    /** Minting policy: Controls the creation of bet tokens This ensures proper initialization of a
      * new bet
      */
    override def mint(
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        tx: TxInfo
    ): Unit =
        // Find all outputs going to this script's address
        tx.outputs.filter(_.address === Address.fromScript(currencySymbol)) match
            case List.Cons(head, tail) =>
                tail match // ???: headOrFail or matchOrFail
                    case List.Nil =>
                        head.datum match // FIME: nested pattern matching
                            case OutputDatum.OutputDatum(datum) =>
                                val BetDatum(player1, player2, oracle, expiration) =
                                    datum.to[BetDatum]
                                require(
                                  tx.signatories.contains(player1),
                                  "Player1 must sign the transaction (they're creating the bet)"
                                )
                                require(
                                  player2.hash.isEmpty,
                                  "Player2 must be empty (no one has joined yet)"
                                )
                                require(
                                  oracle !== player1,
                                  "Oracle cannot be the same as player1 (conflict of interest)"
                                )
                                require(
                                  tx.validRange.entirelyBefore(expiration),
                                  "The bet must have a valid expiration time (after the current time)"
                                )
                            case _ => fail("Datum must be inline")
                    case _ => fail("Output to the script (the bet UTXO) must be single")
            case _ => fail("There's must be an output that goes to the script (the bet UTXO)")

object BettingContract:

    inline def compiled(using scalus.Compiler.Options) = compile(Betting.validate)

    def application: Application = Application
        .ofSingleValidator[VestingDatum, VestingRedeemer](
          "Betting validator",
          "Decentralized two-player betting system with trustless wagering and oracle-based resolution",
          "1.0.0",
          Betting.validate
        )

    def blueprint: Blueprint = application.blueprint

end BettingContract
