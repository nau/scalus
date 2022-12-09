package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.Compiler.fieldAsData
import scalus.Prelude.List
import scalus.Prelude.List.Cons
import scalus.Prelude.List.Nil
import scalus.Prelude.Maybe.*
import scalus.Prelude.*
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.Instances.given
import scalus.ledger.api.v1.*
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.ArbitraryInstances
import scalus.uplc.Constant.Pair
import scalus.uplc.Data.FromData
import scalus.uplc.Data.toData
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.Bool
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{_, given}
import scalus.uplc.*
import scalus.utils.Utils

import java.io.ByteArrayInputStream
import scala.collection.immutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try

enum Expected:
  case Success(term: Term)
  case Failure(description: String)

abstract class BaseValidatorSpec
    extends AnyFunSuite
    with ScalaCheckPropertyChecks
    with ArbitraryInstances {

  protected final def assertSameResult(expected: Expected)(program: Program) = {
    val result1 = PlutusUplcEval.evalFlat(program)
    val result2 = Try(Cek.evalUPLCProgram(program))
    // println(s"$result1 == $result2")
    (expected, result1, result2) match
      case (Expected.Success(term), UplcEvalResult.Success(term1), Success(term2)) =>
        assert(term == term1)
        assert(term == term2)
      case (Expected.Failure(_), UplcEvalResult.UplcFailure(_, err), Failure(e2)) =>
        // println(s"Error: $err and $e2")
        assert(true)
      case _ =>
        fail(
          s"Expected $expected, but got uplc evaluate: $result1\nCek.evalUPLCProgram => $result2"
        )
  }

  protected final def assertUplcEvalResult(expected: Expected)(program: Program) = {
    val result1 = PlutusUplcEval.evalFlat(program)
    (expected, result1) match
      case (Expected.Success(term), UplcEvalResult.Success(term1))   => assert(term == term1)
      case (Expected.Failure(_), UplcEvalResult.UplcFailure(_, err)) => assert(true)
      case _ => fail(s"Expected $expected, but got uplc evaluate: $result1")
  }

  protected final val hoskyMintTxOutRef = TxOutRef(
    TxId(ByteString.fromHex("1ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982")),
    0
  )
  protected final val hoskyMintTxOut = TxOut(
    txOutAddress = Address(
      Credential.PubKeyCredential(
        PubKeyHash(
          hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"
        )
      ),
      Nothing
    ),
    Value.lovelace(BigInt("10000000")),
    Nothing
  )
}
