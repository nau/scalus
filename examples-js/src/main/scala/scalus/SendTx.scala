package scalus

import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.Cek
import scalus.Prelude.AssocMap
import scalus.examples.MintingPolicy
import scalus.builtins.ByteString
import scalus.Compiler.compile
import scalus.uplc.TermDSL.{*, given}

object SendTx:

  val validatorSIR = new SimpleSirToUplcLowering(generateErrorTraces = true)
    .lower(MintingPolicy.compiledOptimizedMintingPolicyScript)
  val tokensSIR =
    compile(AssocMap.singleton(ByteString.fromHex("484f534b59"), BigInt("1000000000000000")))
  val tokens = new SimpleSirToUplcLowering().lower(tokensSIR)
  val evaledTokens = Cek.evalUPLC(tokens)

  def sendFromTxOutRef(txId: ByteString, txOutIdx: BigInt) = {
    val appliedValidator = validatorSIR $ txId $ txOutIdx $ evaledTokens
  }

  def main(args: Array[String]): Unit =

    println(tokensSIR.pretty.render(100))
    println(tokens.pretty.render(100))
    println(evaledTokens.pretty.render(100))
