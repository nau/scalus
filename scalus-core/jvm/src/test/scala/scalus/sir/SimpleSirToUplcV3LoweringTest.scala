package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.*
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.eval.{PlutusVM, Result}

import scala.language.implicitConversions

@org.scalatest.Ignore
class SimpleSirToUplcV3LoweringTest extends AnyFunSuite {
    given PlutusVM = PlutusVM.makePlutusV3VM()

    case class Asdf(
        a: BigInt,
        b: ByteString,
        c: Boolean,
        d: String,
        u: Unit,
        e1: BLS12_381_G1_Element,
        e2: BLS12_381_G2_Element,
        bb: AA
    )
    enum AA:
        case BBB
        case CCC
        case DDD(a: BigInt)

    test("V3 lowering") {
        val sir =
            compile:
                val asdf = new Asdf(
                  123,
                  hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6",
                  false,
                  "asdf",
                  (),
                  Builtins
                      .bls12_381_G1_hashToGroup(ByteString.empty, ByteString.fromString("DST")),
                  Builtins.bls12_381_G2_hashToGroup(
                    ByteString.empty,
                    ByteString.fromString("DST")
                  ),
                  AA.BBB
                )
                //                asdf
                asdf.bb

        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir)
        val term = lower.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

    test("V3 pattern matching") {
        val sir =
            compile:
                val bb: AA = new AA.DDD(123)
                bb match
                    case AA.DDD(i) => i == BigInt(123)
                    case _         => false

        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir)
        val term = lower.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

    test("Asdf") {
        val sir =
            compile:
                val txid = new TxId(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")
                txid match
                    case TxId(hash) => hash
                txid.hash

        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir)
        val term = lower.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

    test("Option") {
        val sir =
            compile:
                new Option.Some(true)

        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir)
        val term = lower.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }

    test("ScriptContext") {
        val sir =
            compile: (ctx: ScriptContext) =>
                ctx.txInfo.validRange

        val ctx = ScriptContext(
          txInfo = TxInfo(
            inputs = prelude.List.empty,
            referenceInputs = prelude.List.empty,
            outputs = prelude.List.empty,
            fee = 0,
            mint = Value.zero,
            certificates = prelude.List.empty,
            withdrawals = AssocMap.empty,
            validRange = Interval.always,
            signatories = prelude.List.empty,
            redeemers = AssocMap.empty,
            data = AssocMap.empty,
            id = TxId(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            votes = AssocMap.empty,
            proposalProcedures = prelude.List.empty,
            currentTreasuryAmount = prelude.Option.None,
            treasuryDonation = prelude.Option.None
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.MintingScript(ByteString.empty)
        )
        import scalus.builtin.Data.toData
        val ctxData = ctx.toData

//        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir)
        val term = lower.lower() $ ctxData.asTerm
        println(term.showHighlighted)
        val Result.Success(t, _, _, _) = term.evaluateDebug: @unchecked
        println(t.showHighlighted)
        println(ctx.txInfo.validRange.toData.asTerm.showHighlighted)
        assert(Term.alphaEq(t, ctx.txInfo.validRange.toData.asTerm))
    }
}
