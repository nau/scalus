package scalus.cardano.ledger

import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.{Codec, Decoder, Encoder, Writer}
import scalus.builtin.{platform, ByteString, PlatformSpecific, given}

case class PlutusV3Script(byteString: ByteString) derives Codec {

    /** Get the script hash for this Plutus V3 script */
    @transient lazy val scriptHash: ScriptHash = Hash(
      platform.blake2b_224(
        ByteString.unsafeFromArray(byteString.bytes.prepended(3))
      )
    )
}
