package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import scalus.builtin.ByteString
import scalus.ledger.api.Timelock
import scalus.builtin.{PlatformSpecific, given}

/** Represents a script in Cardano */
enum Script derives Codec.All:
    /** Native script (timelock) */
    @key(0) case Native(script: Timelock)

    /** Plutus V1 script */
    @key(1) case PlutusV1(bytes: ByteString)

    /** Plutus V2 script */
    @key(2) case PlutusV2(bytes: ByteString)

    /** Plutus V3 script */
    @key(3) case PlutusV3(bytes: ByteString)

    /** Get script language */
    def language: Option[Language] = this match
        case Native(_)   => None
        case PlutusV1(_) => Some(Language.PlutusV1)
        case PlutusV2(_) => Some(Language.PlutusV2)
        case PlutusV3(_) => Some(Language.PlutusV3)

    def scriptHash: ScriptHash =
        this match
            case Native(script)       => Script.nativeScriptHash(script)
            case PlutusV1(byteString) => Script.plutusV1ScriptHash(byteString)
            case PlutusV2(byteString) => Script.plutusV2ScriptHash(byteString)
            case PlutusV3(byteString) => Script.plutusV3ScriptHash(byteString)

object Script:
    def nativeScriptHash(script: Timelock): ScriptHash = Hash(
      summon[PlatformSpecific].blake2b_256(
        ByteString.unsafeFromArray(Cbor.encode(script).toByteArray)
      )
    )

    def plutusV1ScriptHash(byteString: ByteString): ScriptHash =
        plutusScriptHash(byteString, 1)

    def plutusV2ScriptHash(byteString: ByteString): ScriptHash =
        plutusScriptHash(byteString, 2)

    def plutusV3ScriptHash(byteString: ByteString): ScriptHash =
        plutusScriptHash(byteString, 3)

    private def plutusScriptHash(byteString: ByteString, version: Byte): ScriptHash = Hash(
      summon[PlatformSpecific].blake2b_256(
        ByteString.unsafeFromArray(byteString.bytes.prepended(version))
      )
    )
