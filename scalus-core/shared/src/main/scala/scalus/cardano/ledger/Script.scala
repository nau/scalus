package scalus.cardano.ledger

import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import scalus.ledger.api.Timelock

/** Represents a script in Cardano */
enum Script derives Codec.All:
    /** Native script (timelock) */
    @key(0) case Native(script: Timelock)

    /** Plutus V1 script */
    @key(1) case PlutusV1(script: PlutusV1Script)

    /** Plutus V2 script */
    @key(2) case PlutusV2(script: PlutusV2Script)

    /** Plutus V3 script */
    @key(3) case PlutusV3(script: PlutusV3Script)

    /** Get script language */
    def language: Option[Language] = this match
        case Native(_)   => None
        case PlutusV1(_) => Some(Language.PlutusV1)
        case PlutusV2(_) => Some(Language.PlutusV2)
        case PlutusV3(_) => Some(Language.PlutusV3)

    def scriptHash: ScriptHash =
        this match
            case Native(script)   => script.scriptHash
            case PlutusV1(script) => script.scriptHash
            case PlutusV2(script) => script.scriptHash
            case PlutusV3(script) => script.scriptHash
