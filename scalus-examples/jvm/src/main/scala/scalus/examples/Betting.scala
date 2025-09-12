package scalus.examples

import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.cardano.plutus.contract.blueprint.Application
import scalus.cardano.plutus.contract.blueprint.Blueprint
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.{show as _, *}

// https://github.com/cardano-foundation/cardano-template-and-ecosystem-monitoring/blob/main/bet/onchain/aiken/validators/bet.ak

/** Represents the state of a two-player betting game The bet starts with player1 creating it, then
  * player2 can join The oracle decides the winner and triggers the payout.
  *
  * @param player1
  *   The public key hash of the first player (bet creator)
  * @param player2
  *   The public key hash of the second player (None if no one has joined yet)
  * @param oracle
  *   The public key hash of the trusted oracle who will announce the winner
  * @param expiration
  *   The expiration time of the bet (in seconds since the epoch)
  */
case class BetDatum(
    player1: PubKeyHash,
    player2: PubKeyHash,
    oracle: PubKeyHash,
    expiration: PosixTime
) derives FromData,
      ToData

@Compile
object BetDatum

/** Actions that can be performed on the betting contract */
enum Action derives FromData, ToData:
    /** Action for player2 to join an existing bet */
    case Join

    /** Action for the oracle to announce the winner and trigger payout */
    case AnnounceWinner(winner: PubKeyHash)

@Compile
object Action

/** Main betting validator */
@Compile
object Betting extends Validator:
    /** Spending validator: Controls how the bet UTXO can be spent Handles both
      * [[scalus.examples.Action.Join]] and [[scalus.examples.Action.AnnounceWinner]] actions
      */
    override def spend(
        datum: Option[Data],
        redeemer: Data,
        txInfo: TxInfo,
        txOutRef: TxOutRef
    ): Unit =
        // Find the input being spent
        txInfo.findOwnInput(txOutRef) match
            case Some(spentInput) =>
                val input = spentInput.resolved
                redeemer.to[Action] match
                    // Handle player2 joining the bet
                    case Action.Join =>
                        // Verify the input contains the bet token (proves it's a valid bet UTXO)
                        val hasBetToken = input.value.currencySymbols
                            .map(Address.fromScriptHash)
                            .contains(input.address)
                        // Find the continuing output (bet UTXO with updated datum)
                        txInfo.outputs.filter(_.address === input.address) match
                            case List.Cons(continuingOutput, tail) =>
                                tail match // ???: headOrFail or matchOrFail
                                    // Ensure exactly one continuing output
                                    case List.Nil =>
                                        val outputValue = continuingOutput.value
                                        input.datum match // FIME: nested pattern matching
                                            // Extract the current bet state
                                            case OutputDatum.OutputDatum(currentDatum) =>
                                                val currentBetDatum = currentDatum.to[BetDatum]
                                                datum match
                                                    // Extract the updated bet state
                                                    case Some(newDatum) =>
                                                        val newBetDatum = newDatum.to[BetDatum]
                                                        val joiningPlayer = newBetDatum.player2
                                                        // Validation rules for joining:
                                                        require(
                                                          currentBetDatum.player2.hash.length == BigInt(
                                                            0
                                                          ),
                                                          "Current bet must not have a player2 yet"
                                                        )
                                                        require(
                                                          hasBetToken,
                                                          "Input must contain the bet token"
                                                        )
                                                        require(
                                                          txInfo.signatories.contains(
                                                            joiningPlayer
                                                          ),
                                                          "Player2 must sign the transaction"
                                                        )
                                                        require(
                                                          newBetDatum.oracle === currentBetDatum.oracle,
                                                          "Oracle must remain unchanged"
                                                        )
                                                        require(
                                                          newBetDatum.player1 === currentBetDatum.player1,
                                                          "Player1 must remain unchanged"
                                                        )
                                                        require(
                                                          joiningPlayer !== currentBetDatum.player1,
                                                          "Player2 cannot be the same as player1"
                                                        )
                                                        require(
                                                          joiningPlayer !== currentBetDatum.oracle,
                                                          "Player2 cannot be the same as oracle"
                                                        )
                                                        require(
                                                          outputValue.getLovelace == BigInt(
                                                            2
                                                          ) * input.value.getLovelace,
                                                          "The bet amount must double (player2 matches player1's bet)"
                                                        )
                                                        require(
                                                          newBetDatum.expiration === currentBetDatum.expiration,
                                                          "The updated datum must have the same expiration as the current one"
                                                        )
                                                        require(
                                                          txInfo.validRange.entirelyBefore(
                                                            newBetDatum.expiration
                                                          ),
                                                          "Joining must happen before the bet expiration"
                                                        )
                                                    case _ => fail("New datum must be present")
                                            case _ => fail("Datum must be inline")
                                    case _ => fail("Continuing output must be single")
                            case _ => fail("There's must be a continuing output")
                    case Action.AnnounceWinner(winner) => fail("You shall not pass")
            case _ => fail("Spent input must be present")

    /** Minting policy: Controls the creation of bet tokens This ensures proper initialization of a
      * new bet
      */
    override def mint(
        @annotation.unused redeemer: Data,
        currencySymbol: CurrencySymbol,
        tx: TxInfo
    ): Unit =
        // Find all outputs going to this script's address
        tx.outputs.filter(_.address === Address.fromScriptHash(currencySymbol)) match
            case List.Cons(betOutput, tail) =>
                tail match // ???: headOrFail or matchOrFail
                    case List.Nil =>
                        betOutput.datum match // FIME: nested pattern matching
                            case OutputDatum.OutputDatum(datum) =>
                                val BetDatum(player1, player2, oracle, expiration) =
                                    datum.to[BetDatum]
                                require(
                                  tx.signatories.contains(player1),
                                  "Player1 must sign the transaction (they're creating the bet)"
                                )
                                require(
                                  player2.hash.length == BigInt(0), // ???:
                                  // player2.hash.isEmpty: Error: Module not found during linking: scalus.builtin.ByteString
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
