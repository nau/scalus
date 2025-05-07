package scalus.cardano.ledger

import io.bullet.borer.NullOptions.given
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.*
import io.bullet.borer.derivation.key
import scalus.builtin.ByteString
import scalus.ledger.api.Timelock

/** Represents a script in Cardano */
enum Script derives Codec.All:
    /** Native script (timelock) */
    @key(0)
    case Native(script: Timelock)

    /** Plutus V1 script */
    @key(1)
    case PlutusV1(bytes: ByteString)

    /** Plutus V2 script */
    @key(2)
    case PlutusV2(bytes: ByteString)

    /** Plutus V3 script */
    @key(3)
    case PlutusV3(bytes: ByteString)

    /** Get script language */
    def language: Option[Language] = this match
        case Native(_)   => None
        case PlutusV1(_) => Some(Language.PlutusV1)
        case PlutusV2(_) => Some(Language.PlutusV2)
        case PlutusV3(_) => Some(Language.PlutusV3)
