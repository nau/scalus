package scalus.examples

import scalus.*
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.builtin.Builtins
import scalus.builtin.Data
import scalus.builtin.given
import scalus.ledger.api.v1.*
import scalus.sir.SIR
import scalus.uplc.*

@Compile
object OptimizedPreimageValidator {

    /** Validates that the preimage is correct for the given hash and public key hash. The public
      * key hash must be a signatory of the transaction.
      */
    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        // datum is a pair of 2 bytestrings: sha2_256(preimage) and public key hash
        val pair = Builtins.unConstrData(datum).snd
        // get the hash
        inline def hash = Builtins.unBData(pair.head)
        // get the public key hash
        val pkh = Builtins.unBData(pair.tail.head)
        // get the preimage
        inline def preimage = Builtins.unBData(redeemer)
        val checkPubKeyHash = (s: Data) => Builtins.unBData(s) == pkh
        def checkSignatories(sigs: builtin.List[Data]): Unit =
            if checkPubKeyHash(sigs.head) then () else checkSignatories(sigs.tail)
        // get the signatories of the transaction
        inline def sigs =
            Builtins.unListData(fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData))
        checkSignatories(sigs)
        Builtins.sha2_256(preimage) == hash || (throw new RuntimeException("Wrong preimage"))
    }
}

object OptimizedPreimage {
    val compiledOptimizedPreimageValidator: SIR = compile(
      OptimizedPreimageValidator.preimageValidator
    )
    val validator: Term = compiledOptimizedPreimageValidator.toUplc()
    val programV1: Program = Program((1, 0, 0), validator)
    // val cbor = Cbor.encode(flatEncoded).toByteArray
    // val cborHex = Utils.bytesToHex(Cbor.encode(flatEncoded).toByteArray)
    val doubleCborHex: String = Program((1, 0, 0), validator).doubleCborHex
}
