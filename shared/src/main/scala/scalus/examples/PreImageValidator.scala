package scalus.examples

import scalus.*
import scalus.Compiler.{compile, fieldAsData}
import scalus.builtin.Data.fromData
import scalus.builtin.FromDataInstances.given
import scalus.builtin.{Builtins, ByteString, Data, given}
import scalus.ledger.api.v1.*
import scalus.ledger.api.v1.FromDataInstances.given
import scalus.prelude.Prelude.{===, given}
import scalus.prelude.{List, *}
import scalus.sir.SIR
import scalus.uplc.*

@Compile
object OptimizedPreimageValidator {

    def preimageValidator(datum: Data, redeemer: Data, ctxData: Data): Unit = {
        fromData[(ByteString, ByteString)](datum) match
            case (hash, pkh) =>
                val preimage = fromData[ByteString](redeemer)
                val signatories = fromData[List[PubKeyHash]](
                  // deserialize only the signatories from the ScriptContext
                  fieldAsData[ScriptContext](_.txInfo.signatories)(ctxData)
                )

                List.findOrFail(signatories) { sig => sig.hash === pkh }
                if Builtins.sha2_256(preimage) === hash then ()
                else throw new RuntimeException("Wrong preimage")
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
