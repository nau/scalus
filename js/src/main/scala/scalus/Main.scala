package scalus

import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.utils.Utils.*

object Main:

  type PubKeyHash = String
  case class TxInfo(txInfoSignatories: Set[PubKeyHash])
  type ScriptPurpose = Nothing
  case class ScriptContext(
      scriptContextTxInfo: TxInfo,
      scriptContextPurpose: ScriptPurpose
  )
  type Datum = String
  type Redeemer = String
  type ByteString = String

  object Scalus:
    def compile[A](a: A): Dynamic = ???
    def eval[A](a: A, b: Any*): (A, A) = ???
    def mkContext() = ???

  object Builtins:
    def sha256(m: String): String = ???

    def unsafeFromBuiltinData[A](s: String): A = ???

  def validator(
      datum: Datum,
      redeemer: Redeemer,
      ctx: ScriptContext
  ): Boolean =
    val (hash, pkh) = Builtins.unsafeFromBuiltinData[(ByteString, ByteString)](datum)
    Builtins.sha256(redeemer) == hash && ctx.scriptContextTxInfo.txInfoSignatories.contains(pkh)

  val script = Scalus.compile(validator _)
  // this can be run on Node.js or in a browser
  val datum =
    (
      hex"c9d04c9565fc665c80681fb1d829938026871f66e14f501e08531df66938a789",
      hex"dfc1ed82c9fc06409cc0d137e561fef37d1072e415e637ca54a2d044f3777da9"
    )
  val redeemer = "Test"
  val (result, budget) = Scalus.eval(script, datum, redeemer, Scalus.mkContext())

  def main(args: Array[String]): Unit =
    println("Scalus Hello World")
    val h = Const(DefaultUni.asConstant("Hello"))
    val id = LamAbs("x", Var("x"))
    val app = Apply(id, h)
    println(Cek.evalUPLC(app))
