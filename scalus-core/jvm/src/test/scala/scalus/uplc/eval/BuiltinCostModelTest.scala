package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.cardano.ledger.Language
import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.ledger.babbage.*
import upickle.default.*

import java.nio.charset.StandardCharsets

class BuiltinCostModelTest extends AnyFunSuite:
    test("BuiltinCostModel JSON reader from Cardano Protocol Parameters") {
        val input = this.getClass.getResourceAsStream("/protocol-params.json")
        val pparams = read[ProtocolParams](input)
        testReadingCostModelParams(pparams)
    }

    test("BuiltinCostModel JSON reader from Blockfrost Protocol Parameters epoch 507") {
        val input = this.getClass.getResourceAsStream("/blockfrost-params-epoch-507.json")
        val json = new String(input.readAllBytes, StandardCharsets.UTF_8)
        val pparams = ProtocolParams.fromBlockfrostJson(json)
        testReadingCostModelParams(pparams)
    }

    test("BuiltinCostModel JSON reader from Blockfrost Protocol Parameters epoch 544") {
        val input = this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        val json = new String(input.readAllBytes, StandardCharsets.UTF_8)
        val pparams = ProtocolParams.fromBlockfrostJson(json)
        testReadingCostModelParams(pparams)
    }

    private def testReadingCostModelParams(pparams: ProtocolParams): Unit = {
        val v1 = pparams.costModels(Language.PlutusV1.toString)
        val v2 = pparams.costModels(Language.PlutusV2.toString)
        val v3 = pparams.costModels(Language.PlutusV3.toString)
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        BuiltinCostModel.fromPlutusParams(paramsV1, Language.PlutusV1, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV2, Language.PlutusV2, BuiltinSemanticsVariant.B)
        BuiltinCostModel.fromPlutusParams(paramsV3, Language.PlutusV3, BuiltinSemanticsVariant.C)
    }
