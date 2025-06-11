package scalus.cardano.ledger

import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.{Codec, Decoder, Encoder, Writer}

/** Represents a voting procedure in the Cardano blockchain governance system.
  *
  * A voting procedure consists of a vote and an optional anchor with additional metadata.
  *
  * @param vote
  *   The vote (Yes, No, or Abstain)
  * @param anchor
  *   Optional anchor with additional metadata
  */
case class VotingProcedure(
    vote: Vote,
    anchor: Option[Anchor]
) derives Codec
