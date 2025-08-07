package scalus.sir

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.*
import scalus.builtin.ByteString.hex
import scalus.ledger.api.v3.*
import scalus.prelude.*
// note that this import is needed, compiler warning is a false positive
import scalus.builtin.Data.toData
import scalus.uplc.*
import scalus.uplc.Term.*
import scalus.uplc.eval.{PlutusVM, Result}

import scala.language.implicitConversions

class S3LoweringDataAccessTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    case class Asdf(
        a: BigInt,
        b: ByteString,
        c: Boolean,
        d: String,
        // u: Unit,  // have np data represemtatom.  TODO: enble
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
                  // (),
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

        // println(sir.showHighlighted)
        val lower = SirToUplcV3Lowering(sir)
        val term = lower.lower()
        // println(term.showHighlighted)
        val r = term.evaluateDebug
    }

    test("pattern match on DDD enum") {
        val sir =
            compile:
                val bb: AA = new AA.DDD(123)
                bb match
                    case AA.DDD(i) => i == BigInt(123)
                    case _         => false

        // println(sir.showHighlighted)
        val lower = SirToUplcV3Lowering(sir)
        val term = lower.lower()
        // println(term.showHighlighted)
        val Result.Success(result, _, _, _) = term.evaluateDebug: @unchecked

        assert(result == Term.Const(Constant.Bool(true)))
    }

    test("pattern match on txId") {
        val sir =
            compile:
                val txid = new TxId(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")
                txid match
                    case TxId(hash) => hash
                txid.hash

        // println(sir.showHighlighted)
        val lower = SirToUplcV3Lowering(sir)
        val term = lower.lower()
        // println(term.showHighlighted)
        val Result.Success(r, _, _, _) = term.evaluateDebug: @unchecked
        assert(
          r == Term.Const(
            Constant.ByteString(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")
          )
        )
    }

    test("Option") {
        val sir =
            compile:
                new Option.Some(true)

        // println(sir.showHighlighted)
        val lower = SirToUplcV3Lowering(sir)
        val term = lower.lower()
        // println(term.showHighlighted)
        val r = term.evaluateDebug
        assert(r.isSuccess)
    }

    test("get txInfop.validRange from ScriptContext") {
        val sir =
            compile: (ctxData: Data) =>
                val ctx = Data.fromData[ScriptContext](ctxData)
                ctx.txInfo.validRange.toData

        val ctx = ScriptContext(
          txInfo = TxInfo(
            inputs = prelude.List.empty,
            referenceInputs = prelude.List.empty,
            outputs = prelude.List.empty,
            fee = 0,
            mint = Value.zero,
            certificates = prelude.List.empty,
            withdrawals = SortedMap.empty,
            validRange = Interval.always,
            signatories = prelude.List.empty,
            redeemers = SortedMap.empty,
            data = SortedMap.empty,
            id = TxId(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            votes = SortedMap.empty,
            proposalProcedures = prelude.List.empty,
            currentTreasuryAmount = prelude.Option.None,
            treasuryDonation = prelude.Option.None
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.MintingScript(ByteString.empty)
        )
        import scalus.builtin.Data.toData
        val ctxData = ctx.toData

        // println(sir.showHighlighted)
        val lower = SirToUplcV3Lowering(sir)
        val term = lower.lower() $ ctxData.asTerm
        // println(term.showHighlighted)
        val Result.Success(t, _, _, _) = term.evaluateDebug: @unchecked
        // println(t.showHighlighted)
        // println(ctx.txInfo.validRange.toData.asTerm.showHighlighted)
        assert(Term.alphaEq(t, ctx.txInfo.validRange.toData.asTerm))
    }

}
