package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.ledger.babbage.*
import upickle.default.*

class BuiltinCostModelTest extends AnyFunSuite:
    test("BuiltinCostModel JSON reader from Cardano Protocol Parameters") {
        val input = this.getClass.getResourceAsStream("/protocol-params.json")
        val pparams = read[ProtocolParams](input)
        testReadingCostModelParams(pparams)
    }

    test("BuiltinCostModel JSON reader from Blockfrost Protocol Parameters epoch 507") {
        val input = this.getClass.getResourceAsStream("/blockfrost-params-epoch-507.json")
        val pparams = read[ProtocolParams](input)(using ProtocolParams.blockfrostParamsRW)
        testReadingCostModelParams(pparams)
    }

    test("BuiltinCostModel JSON reader from Blockfrost Protocol Parameters epoch 544") {
        val input = this.getClass.getResourceAsStream("/blockfrost-params-epoch-544.json")
        val pparams = read[ProtocolParams](input)(using ProtocolParams.blockfrostParamsRW)
        testReadingCostModelParams(pparams)
    }

    private def testReadingCostModelParams(pparams: ProtocolParams): Unit = {
        val v1 = pparams.costModels("PlutusV1")
        val v2 = pparams.costModels("PlutusV2")
        val v3 = pparams.costModels("PlutusV3")
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        val paramsV1Map = writeJs(paramsV1).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val paramsV2Map = writeJs(paramsV2).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val paramsV3Map = writeJs(paramsV3).obj.map { case (k, v) => (k, v.num.toLong) }.toMap

//        println(paramsV1.getClass.getDeclaredFields.map(_.getName).mkString("\n"))
//        assert(
//          v1.size == paramsV1.numberOfParams,
//          s"Expected ${paramsV1.numberOfParams} parameters for PlutusV1, got ${v1.size}. You may need to update the cost model."
//        )
//        assert(
//          v2.size == paramsV2.numberOfParams,
//          s"Expected ${paramsV2.numberOfParams} parameters for PlutusV2, got ${v2.size}. You may need to update the cost model."
//        )
//        assert(
//          v3.size == paramsV3.numberOfParams,
//          s"Expected ${paramsV3.numberOfParams} parameters for PlutusV3, got ${v3.size}. You may need to update the cost model."
//        )

        // check that we can read the parameters
        BuiltinCostModel.fromCostModelParams(PlutusV1, BuiltinSemanticsVariant.B, paramsV1Map)
        BuiltinCostModel.fromCostModelParams(PlutusV2, BuiltinSemanticsVariant.B, paramsV2Map)
        BuiltinCostModel.fromCostModelParams(PlutusV3, BuiltinSemanticsVariant.C, paramsV3Map)
    }
