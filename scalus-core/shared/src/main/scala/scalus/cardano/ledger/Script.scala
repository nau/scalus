package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import scalus.builtin.{platform, ByteString}
import scalus.ledger.api.Timelock

/** Represents a script in Cardano */
sealed trait Script {

    def scriptHash: ScriptHash
}

sealed trait PlutusScript extends Script {
    def script: ByteString

    /** Get script language */
    def language: Language
}

object Script {

    /** Native script (timelock) */
    @key(0) final case class Native(script: Timelock) extends Script derives Codec {

        /** Get the script hash for this native script */
        @transient lazy val scriptHash: ScriptHash = script.scriptHash
    }

    /** Plutus V1 script */
    @key(1) final case class PlutusV1(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V1 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(1 +: script.bytes))
        )

        def language: Language = Language.PlutusV1
    }

    /** Plutus V2 script */
    @key(2) final case class PlutusV2(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V2 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(2 +: script.bytes))
        )
        def language: Language = Language.PlutusV2
    }

    /** Plutus V3 script */
    @key(3) final case class PlutusV3(override val script: ByteString) extends PlutusScript
        derives Codec {

        /** Get the script hash for this Plutus V3 script */
        @transient lazy val scriptHash: ScriptHash = Hash(
          platform.blake2b_224(ByteString.unsafeFromArray(3 +: script.bytes))
        )
        def language: Language = Language.PlutusV3
    }

    given Codec[Script] = deriveCodec
}
