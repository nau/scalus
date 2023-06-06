package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.builtins.ByteString.given
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.*
import scalus.prelude.List.Cons
import scalus.prelude.List.Nil
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.sir.SimpleSirToUplcLowering
import scalus.uplc.*

@Compile
object PubKeyValidator {
  def validator(redeemer: Unit, datum: Unit, ctx: Data) = {
    val txinfo = Builtins.unsafeDataAsConstr(Builtins.unsafeDataAsConstr(ctx).snd.head).snd
    val signatories = Builtins.unsafeDataAsList(txinfo.tail.tail.tail.tail.tail.tail.tail.head)

    def findSignatureOrFail(sigs: builtins.List[Data]): Unit =
      if signatories.isEmpty then throw new RuntimeException("Signature not found")
      else if Builtins.unsafeDataAsB(signatories.head) === hex"deadbeef"
      then ()
      else findSignatureOrFail(signatories.tail)

    findSignatureOrFail(signatories)
  }
}

class PubKeyValidatorSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
  test("PubKey Validator example") {
    val scriptContext =
      ScriptContext(
        TxInfo(
          Nil,
          Nil,
          Value.zero,
          Value.zero,
          Nil,
          Nil,
          Interval.always,
          Cons(PubKeyHash(hex"deadbeef"), Nil),
          Nil,
          TxId(hex"bb")
        ),
        ScriptPurpose.Spending(TxOutRef(TxId(hex"deadbeef"), 0))
      )

    val compiled = compile { PubKeyValidator.validator }

    // println(compiled.pretty.render(80))
    val term = new SimpleSirToUplcLowering().lower(compiled)
    val flatBytesLen = ProgramFlatCodec.encodeFlat(Program(version = (1, 0, 0), term = term)).length
//    println(Utils.bytesToHex(flatBytes))
    // println(term.pretty.render(80))
    assert(flatBytesLen == 131)
    import Data.*
    import DefaultUni.asConstant
    import TermDSL.{*, given}
//    println(scriptContext.toData)
    val appliedValidator = term $ asConstant(()) $ asConstant(()) $ scriptContext.toData
    assert(
      Cek.evalUPLC(appliedValidator) == Term.Const(asConstant(()))
    )
    assert(
      PubKeyValidator.validator((), (), scriptContext.toData) == ()
    )
  }
