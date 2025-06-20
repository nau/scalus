package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.{Codec, Decoder, Encoder, Writer}
import scalus.builtin.{ByteString, PlatformSpecific, given}

case class PlutusV1Script(byteString: ByteString) derives Codec {

    /** Get the script hash for this Plutus V1 script */
    @transient lazy val scriptHash: ScriptHash = Hash(
      summon[PlatformSpecific].blake2b_256(
        ByteString.unsafeFromArray(byteString.bytes.prepended(1))
      )
    )
}
