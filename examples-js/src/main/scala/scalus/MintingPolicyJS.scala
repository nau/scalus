package scalus

import io.bullet.borer.Cbor
import scalus.Compiler.compile
import scalus.builtins.ByteString
import scalus.examples.MintingPolicy
import scalus.prelude.AssocMap
import scalus.uplc.Cek
import scalus.uplc.Data
import scalus.uplc.Program
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.TermDSL.{_, given}
import scalus.utils.Utils

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportAll
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
    val evaledTokens = Cek.evalUPLC(tokensSIR.toUplc())
    val txId = ByteString.fromHex(txIdHex)
    val tokens = evaledTokens $ ByteString.fromHex(tokenNameHex) $ amount
    // val appliedValidator = alwaysokTerm
    val appliedValidator =
      validatorSIR $ txId $ txOutIdx $ tokens
    val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), appliedValidator))
    val cbor = Cbor.encode(flatEncoded).toByteArray
    Utils.bytesToHex(Cbor.encode(cbor).toByteArray)
  }
