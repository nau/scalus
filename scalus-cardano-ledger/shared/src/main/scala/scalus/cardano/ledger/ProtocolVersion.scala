package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*

/** Represents a Cardano protocol version */
case class ProtocolVersion(
    /** Major version (1-10) */
    major: Int,

    /** Minor version */
    minor: Int
) derives Codec:
    require(major >= 1 && major <= 10, s"Major version must be between 1 and 10, got $major")
    require(minor >= 0, s"Minor version must be non-negative, got $minor")
