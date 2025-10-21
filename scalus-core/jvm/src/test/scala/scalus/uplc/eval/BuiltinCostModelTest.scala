package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.cardano.ledger.{Language, ProtocolParams}
import scalus.uplc.*

class BuiltinCostModelTest extends AnyFunSuite:
    test("BuiltinCostModel from Cardano Protocol Parameters") {
        val pparams = ProtocolParams.fromCardanoCliJson(
          this.getClass.getResourceAsStream("/protocol-params.json")
        )
        val v1 = pparams.costModels.models(Language.PlutusV1.languageId)
        val v2 = pparams.costModels.models(Language.PlutusV2.languageId)
        val v3 = pparams.costModels.models(Language.PlutusV3.languageId)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
        assert(v1.size == 166)
        assert(v2.size == 175)
        assert(v3.size == 297)
    }

    test("BuiltinCostModel from Blockfrost pre-Plomin HF Protocol Parameters epoch 507") {
        val pparams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-507.json")
        )
        val v1 = pparams.costModels.models(Language.PlutusV1.languageId)
        val v2 = pparams.costModels.models(Language.PlutusV2.languageId)
        val v3 = pparams.costModels.models(Language.PlutusV3.languageId)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
        assert(v1.size == 166)
        assert(v2.size == 175)
        assert(v3.size == 251)
        assert(paramsV1.`addInteger-cpu-arguments-intercept` == 100788)
        // not available pre-Plomin HF
        assert(paramsV2.`integerToByteString-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV2.`byteStringToInteger-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV3.`andByteString-cpu-arguments-slope1` == 300_000_000L)
    }

    test("BuiltinCostModel from Blockfrost Plomin HF Protocol Parameters epoch 544") {
        val pparams = ProtocolParams.fromBlockfrostJson(
          this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        )
        val v1 = pparams.costModels.models(Language.PlutusV1.languageId)
        val v2 = pparams.costModels.models(Language.PlutusV2.languageId)
        val v3 = pparams.costModels.models(Language.PlutusV3.languageId)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
        assert(v1.size == 166)
        assert(v2.size == 175)
        assert(v3.size == 297)
        assert(paramsV1.`addInteger-cpu-arguments-intercept` == 100788)
        // for some reason, these values are absent in Blockfrost params
        assert(paramsV2.`integerToByteString-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV2.`byteStringToInteger-cpu-arguments-c0` == 300_000_000L)
        assert(paramsV3.`andByteString-cpu-arguments-slope1` == 726)
    }
