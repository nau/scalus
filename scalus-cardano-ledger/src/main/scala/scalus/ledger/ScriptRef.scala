package scalus.ledger

import io.bullet.borer.Tag.EmbeddedCBOR
import io.bullet.borer.*

/** Represents a reference to a script in Cardano */
case class ScriptRef(script: Script)

object ScriptRef:
    /** CBOR encoder for ScriptRef */
    given Encoder[ScriptRef] with
        def write(w: Writer, value: ScriptRef): Writer =
            // Tag 24 is used for embedded CBOR
            w.writeTag(EmbeddedCBOR)

            // Serialize the script to CBOR bytes
            val scriptBytes = Cbor.encode(value.script).toByteArray

            // Write the bytes
            w.writeBytes(scriptBytes)
            w

    /** CBOR decoder for ScriptRef */
    given Decoder[ScriptRef] with
        def read(r: Reader): ScriptRef =
            // Check for tag 24 (embedded CBOR)
            val tag = r.readTag()
            if tag != EmbeddedCBOR then
                r.validationFailure(s"Expected tag 24 for ScriptRef, got $tag")

            // Read the embedded CBOR bytes
            val bytes: Array[Byte] = r.readBytes()

            // Parse the bytes as a Script
            val script = Cbor.decode(bytes).to[Script].value

            ScriptRef(script)
