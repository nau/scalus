package scalus.examples

import scalus.Compile
import scalus.Compiler.fieldAsData
import scalus.builtin
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.ByteString.given
import scalus.builtin.Data

@Compile
object PubKeyValidator {
    def validator(redeemer: Unit, datum: Unit, ctx: Data) = {
        val txinfo = unConstrData(unConstrData(ctx).snd.head).snd
        val signatories = unListData(txinfo.tail.tail.tail.tail.tail.tail.tail.head)

        def findSignatureOrFail(sigs: builtin.List[Data]): Unit =
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if unBData(signatories.head) == hex"deadbeef"
            then ()
            else findSignatureOrFail(signatories.tail)

        findSignatureOrFail(signatories)
    }

    def validatorV2(redeemer: Unit, datum: Unit, ctx: Data) = {
        import scalus.ledger.api.v2.ScriptContext
        val signatories = unListData(fieldAsData[ScriptContext](_.txInfo.signatories)(ctx))

        def findSignatureOrFail(sigs: builtin.List[Data]): Unit =
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if unBData(signatories.head) == hex"deadbeef"
            then ()
            else findSignatureOrFail(signatories.tail)

        findSignatureOrFail(signatories)
    }
}
