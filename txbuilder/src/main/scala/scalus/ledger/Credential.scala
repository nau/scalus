package scalus.ledger

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scalus.builtin.ByteString

/** Represents a credential in the Cardano blockchain. A credential can be either a key hash or a
  * script hash.
  */
enum Credential:
    /** Key hash credential */
    case KeyHash(hash: Hash28)

    /** Script hash credential */
    case ScriptHash(hash: Hash28)

    /** Check if this credential is a key hash */
    def isKeyHash: Boolean = this match
        case KeyHash(_) => true
        case _          => false

    /** Check if this credential is a script hash */
    def isScriptHash: Boolean = this match
        case ScriptHash(_) => true
        case _             => false

object Credential:
    /** CBOR encoder for Credential */
    given Encoder[Credential] with
        def write(w: Writer, value: Credential): Writer = value match
            case Credential.KeyHash(hash) =>
                w.writeArrayHeader(2)
                    .writeInt(0)
                    .writeBytes(hash.bytes.bytes)

            case Credential.ScriptHash(hash) =>
                w.writeArrayHeader(2)
                    .writeInt(1)
                    .writeBytes(hash.bytes.bytes)

    /** CBOR decoder for Credential */
    given Decoder[Credential] with
        def read(r: Reader): Credential =
            r.readArrayHeader()
            val tag = r.readInt()

            tag match
                case 0 =>
                    val hash = ByteString.unsafeFromArray(r.readBytes())
                    Credential.KeyHash(Hash28(hash))
                case 1 =>
                    val hash = ByteString.unsafeFromArray(r.readBytes())
                    Credential.ScriptHash(Hash28(hash))
                case _ =>
                    r.validationFailure(s"Invalid credential type: $tag, expected 0 or 1")
