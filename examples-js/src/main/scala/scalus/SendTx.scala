package scalus

import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Cek
import scalus.prelude.AssocMap
import scalus.examples.MintingPolicy
import scalus.builtins.ByteString
import scalus.Compiler.compile
import scalus.uplc.TermDSL.{*, given}
import scala.scalajs.js.annotation.JSExportTopLevel
import scalus.uplc.ProgramFlatCodec
import io.bullet.borer.Cbor
import scalus.uplc.Program
import scala.scalajs.js.annotation.JSExport
import scalus.utils.Utils
import scala.scalajs.js.annotation.JSExportAll
import scalus.uplc.Data

@JSExportTopLevel("SendTx")
object SendTx:

  val validatorSIR = new SimpleSirToUplcLowering(generateErrorTraces = true)
    .lower(MintingPolicy.compiledOptimizedMintingPolicyScript)

  val alwaysok = compile((redeemer: Data, ctx: Data) => ())
  val alwaysokTerm = new SimpleSirToUplcLowering().lower(alwaysok)

  @JSExportAll
  case class Asdf(cbor: Array[Byte], doubleCbor: String)
  @JSExport
  def getPlutusScriptCborFromTxOutRef(
      txIdHex: String,
      txOutIdx: Int,
      tokenNameHex: String,
      amount: Int
  ) = {
    val tokensSIR = compile((tokenNameHex: ByteString, amount: BigInt) =>
      AssocMap.singleton(tokenNameHex, amount)
    )
    val evaledTokens = Cek.evalUPLC(new SimpleSirToUplcLowering().lower(tokensSIR))
    val txId = ByteString.fromHex(txIdHex)
    val tokens = evaledTokens $ ByteString.fromHex(tokenNameHex) $ amount
    // val appliedValidator = alwaysokTerm
    val appliedValidator =
      validatorSIR $ txId $ txOutIdx $ tokens
    val flatEncoded = ProgramFlatCodec.encodeFlat(Program((1, 0, 0), appliedValidator))
    val cbor = Cbor.encode(flatEncoded).toByteArray
    Asdf(cbor = cbor, doubleCbor = Utils.bytesToHex(Cbor.encode(cbor).toByteArray))
  }

  def main(args: Array[String]): Unit =
    println(validatorSIR.pretty.render(100))

    // println(tokensSIR.pretty.render(100))
    // println(tokens.pretty.render(100))
    // println(evaledTokens.pretty.render(100))
