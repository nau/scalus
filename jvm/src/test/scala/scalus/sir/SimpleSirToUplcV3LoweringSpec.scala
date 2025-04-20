package scalus.sir

import org.scalacheck.Arbitrary
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.{compile, compileDebug}
import scalus.builtin.ByteString.*
import scalus.builtin.Data.{toData, toData1}
import scalus.builtin.{*, given}
import scalus.ledger.api.v3.*
import scalus.prelude.{List, *}
import scalus.prelude.Option.Some
import scalus.uplc.DefaultFun.BData
import scalus.uplc.Term
import scalus.uplc.Term.*
import scalus.uplc.eval.{ExBudget, ExCPU, ExMemory, PlutusVM, Result}
import scalus.uplc.transform.{CaseConstrApply, EtaReduce, ForcedBuiltinsExtractor, Inliner}

import scala.language.implicitConversions

class SimpleSirToUplcV3LoweringSpec extends AnyFunSuite {
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

    /*test("V3 lowering") {
        val sir =
            compile:
                val asdf = Asdf(
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
                val bb: AA = AA.DDD(123)
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
                val txid = TxId(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6")
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
                Option.Some(true)

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
            withdrawals = AssocMap(prelude.List.empty),
            validRange = Interval.always,
            signatories = prelude.List.empty,
            redeemers = AssocMap(prelude.List.empty),
            data = AssocMap(prelude.List.empty),
            id = TxId(hex"61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6"),
            votes = AssocMap(prelude.List.empty),
            proposalProcedures = prelude.List.empty,
            currentTreasuryAmount = prelude.Option.None,
            treasuryDonation = prelude.Option.None
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.MintingScript(ByteString.empty)
        )
        import scalus.builtin.Data.toData
        import scalus.ledger.api.v1.ToDataInstances.given
        import scalus.ledger.api.v3.ToDataInstances.given
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
     */
    test("Hello Cardano") {
        import scalus.ledger.api.v1.FromDataInstances.given
        import scalus.ledger.api.v1.ToDataInstances.given
        import scalus.builtin.FromDataInstances.given
        val sir =
            compile { (ctx: ScriptContext) =>
                ctx.scriptInfo match
                    case ScriptInfo.SpendingScript(_, datum) =>
                        val Some(ownerData) = datum: @unchecked
                        val signed = ctx.txInfo.signatories.find(_.toData1 == ownerData)
                        signed.orFail("Must be signed")
                        val saysHello = ctx.redeemer.to1[String] == "Hello, Cardano!"
                        require(saysHello, "Invalid redeemer")
                    case _ => scalus.prelude.fail("Invalid script info")
            }

        import scalus.builtin.Data.toData
        import scalus.builtin.ToDataInstances.given
        import scalus.ledger.api.v1.ToDataInstances.given
        import scalus.ledger.api.v3.ToDataInstances.given

        val ownerPubKey = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val message = "Hello, Cardano!".toData
        val ownInput =
            TxInInfo(
              outRef = TxOutRef(
                TxId(hex"deadbeef"),
                0
              ),
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(hex"deadbeef"),
                  Option.None
                ),
                value = Value.zero
              )
            )
        val ctx = ScriptContext(
          txInfo = TxInfo(
            inputs = prelude.List(ownInput),
            fee = 188021,
            signatories = prelude.List(ownerPubKey),
            id = TxId(hex"deadbeef")
          ),
          redeemer = message,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(ownerPubKey.toData)
          )
        )
        val ctxData = ctx.toData

        println(sir.showHighlighted)
        val lower = SimpleSirToUplcV3Lowering(sir, generateErrorTraces = true).lower()
        val opt = lower
            |> EtaReduce.apply
            |> Inliner.apply
            |> CaseConstrApply.apply
            |> ForcedBuiltinsExtractor.apply

        val length = opt.plutusV3.cborEncoded.length
        assert(length == 487)
        val applied = opt.plutusV3 $ ctxData.asTerm
        val result = applied.evaluateDebug
        assert(result.isSuccess)
        assert(result.budget == ExBudget(ExCPU(11433264), ExMemory(31993)))
    }

    test("fromData") {
        import scalus.ledger.api.v3.FromDataInstances.given
        val sir = compileDebug: (d: Data) =>
            d.to1[ScriptContext].txInfo

        println(sir.showHighlighted)
        val uplc = SimpleSirToUplcV3Lowering(sir).lower()
        println(uplc.showHighlighted)
    }

    test("toData primitive") {
        import scalus.builtin.ToDataInstances.given
        val sir = compileDebug: (d: ByteString) =>
            d.toData1

        val uplc = SimpleSirToUplcV3Lowering(sir).lower()
        assert(uplc == LamAbs("d", Builtin(BData) $ vr"d"))
    }

    test("toData case class") {
        import scalus.ledger.api.v3.ToDataInstances.given
        val sir = compileDebug: (d: ScriptContext) =>
            d.toData1

        val uplc = SimpleSirToUplcV3Lowering(sir).lower()
        assert(uplc == LamAbs("d", vr"d"))
    }

    test("prelude List") {
        import scalus.ledger.api.v3.ToDataInstances.given
        import scalus.builtin.ToDataInstances.given
        val sir = compileDebug: (l: prelude.List[Data]) =>
            l match
//                case List.Nil              => ().toData1
                case List.Cons(head, tail) => head
                case _                     => prelude.fail("Invalid list")
        println(sir.showHighlighted)
        val uplc = SimpleSirToUplcV3Lowering(sir).lower()
        val opt = uplc |> EtaReduce.apply
            |> Inliner.apply
            |> CaseConstrApply.apply
            |> ForcedBuiltinsExtractor.apply
        println(opt.showHighlighted)
        println(opt.plutusV3.cborEncoded.length)
        val list = List(true).toData
        val applied = uplc.plutusV3 $ list
        println(applied.showHighlighted)
        println(applied.term.evaluateDebug)
    }

}
