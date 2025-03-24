package scalus.ledger

import scalus.builtin.ByteString
import scalus.ledger.api.Timelock
import io.bullet.borer.{Decoder, Encoder, Reader, Writer}

/** Represents a script in Cardano */
enum Script:
    /** Native script (timelock) */
    case Native(script: Timelock)

    /** Plutus V1 script */
    case PlutusV1(bytes: ByteString)

    /** Plutus V2 script */
    case PlutusV2(bytes: ByteString)

    /** Plutus V3 script */
    case PlutusV3(bytes: ByteString)

    /** Get script language */
    def language: Option[Language] = this match
        case Native(_)   => None
        case PlutusV1(_) => Some(Language.PlutusV1)
        case PlutusV2(_) => Some(Language.PlutusV2)
        case PlutusV3(_) => Some(Language.PlutusV3)


object Script:
    /** CBOR encoder for Script */
    given Encoder[Script] with
        def write(w: Writer, value: Script): Writer =
            w.writeArrayHeader(2)
            value match
                case Script.Native(script) =>
                    w.writeInt(0)
                    Timelock.given_Encoder_Timelock.write(w, script)

                case Script.PlutusV1(bytes) =>
                    w.writeInt(1)
                    w.writeBytes(bytes.bytes)

                case Script.PlutusV2(bytes) =>
                    w.writeInt(2)
                    w.writeBytes(bytes.bytes)

                case Script.PlutusV3(bytes) =>
                    w.writeInt(3)
                    w.writeBytes(bytes.bytes)
            w

    /** CBOR decoder for Script */
    given Decoder[Script] with
        def read(r: Reader): Script =
            val size = r.readArrayHeader()
            if size != 2 then r.validationFailure(s"Expected 2 elements for Script, got $size")

            val tag = r.readInt()
            tag match
                case 0 => Script.Native(Timelock.given_Decoder_Timelock.read(r))

                case 1 =>
                    val bytes = ByteString.unsafeFromArray(r.readBytes())
                    // In real implementation, we might validate minimum/maximum sizes
                    Script.PlutusV1(bytes)

                case 2 =>
                    val bytes = ByteString.unsafeFromArray(r.readBytes())
                    Script.PlutusV2(bytes)

                case 3 =>
                    val bytes = ByteString.unsafeFromArray(r.readBytes())
                    Script.PlutusV3(bytes)

                case other =>
                    r.validationFailure(s"Invalid Script tag: $tag")
