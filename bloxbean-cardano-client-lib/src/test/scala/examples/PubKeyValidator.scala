package scalus.examples

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.prelude.log

@Compile
object PubKeyValidator {
    def validator(datum: Unit, redeemer: Unit, ctx: Data) = {
        val txinfo = unConstrData(unConstrData(ctx).snd.head).snd
        val signatories = unListData(txinfo.tail.tail.tail.tail.tail.tail.tail.head)

        def findSignatureOrFail(sigs: builtin.List[Data]): Unit =
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if unBData(signatories.head) == hex"deadbeef"
            then ()
            else findSignatureOrFail(signatories.tail)

        findSignatureOrFail(signatories)
    }

    inline def validatorV2(inline pubKey: ByteString)(datum: Unit, redeemer: Unit, ctx: Data) = {
        import scalus.ledger.api.v2.ScriptContext
        val signatories = ctx.field[ScriptContext](_.txInfo.signatories).toList
        log("got signatories")

        def findSignatureOrFail(sigs: builtin.List[Data]): Unit =
            log("check sigs")
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if signatories.head.toByteString == pubKey
            then log("found!")
            else trace("nope")(findSignatureOrFail(signatories.tail))

        findSignatureOrFail(signatories)
    }

    inline def validatorV3(inline pubKey: ByteString)(ctx: Data) = {
        import scalus.ledger.api.v3.ScriptContext
        val signatories = ctx.field[ScriptContext](_.txInfo.signatories).toList
        log("got signatories")

        def findSignatureOrFail(sigs: builtin.List[Data]): Unit =
            log("check sigs")
            if signatories.isEmpty then throw new RuntimeException("Signature not found")
            else if signatories.head.toByteString == pubKey
            then log("found!")
            else trace("nope")(findSignatureOrFail(signatories.tail))

        findSignatureOrFail(signatories)
    }
}
