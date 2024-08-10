package scalus

import scalus.Compiler.compile
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.examples.MintingPolicy
import scalus.prelude.AssocMap
import scalus.uplc.Program
import scalus.uplc.TermDSL.{_, given}
import scalus.uplc.eval.VM

import scala.language.implicitConversions
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("MintingPolicyJS")
object MintingPolicyJS:

    val validatorSIR =
        MintingPolicy.compiledOptimizedMintingPolicyScript.toUplc(generateErrorTraces = true)

    val alwaysok = compile((redeemer: Data, ctx: Data) => ())
    val alwaysokTerm = alwaysok.toUplc()

    @JSExport
    def getPlutusScriptCborFromTxOutRef(
        txIdHex: String,
        txOutIdx: Int,
        tokenNameHex: String,
        amount: Int
    ): String = {
        val tokensSIR = compile((tokenNameHex: ByteString, amount: BigInt) =>
            AssocMap.singleton(tokenNameHex, amount)
        )
        val evaledTokens = tokensSIR.toUplc().eval
        val txId = ByteString.fromHex(txIdHex)
        val tokens = evaledTokens $ ByteString.fromHex(tokenNameHex) $ amount
        // val appliedValidator = alwaysokTerm
        val appliedValidator =
            validatorSIR $ txId $ txOutIdx $ tokens

        Program((1, 0, 0), appliedValidator).doubleCborHex
    }
