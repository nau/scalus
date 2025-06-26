package scalus.cardano.ledger

import io.bullet.borer.Cbor
import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.{ByteString, Data, PlatformSpecific}
import scalus.utils.Hex.toHex

import scala.collection.immutable.ListMap

class CostModelsTest extends AnyFunSuite {

    test("getLanguageViewEncoding should handle empty cost models") {
        val costModels = CostModels(Map.empty)
        val encoding = costModels.getLanguageViewEncoding
        assert(encoding.isEmpty)
    }

    test("getLanguageViewEncoding should handle PlutusV1 with double-bagging") {
        // Test PlutusV1 cost model encoding with double-bagging
        val plutusV1CostModel = IndexedSeq(1L, 2L, 3L, 4L, 5L)
        val costModels = CostModels(Map(0 -> plutusV1CostModel))
        
        val encoding = costModels.getLanguageViewEncoding
        
        // Verify the encoding is not empty
        assert(encoding.nonEmpty)
        
        // Manually construct expected encoding for verification
        val langId = 0
        val costModelArray = plutusV1CostModel.map(_.toInt).toArray
        
        // PlutusV1: Double-bagged encoding
        val singleEncodedLang = Cbor.encode(langId).toByteArray
        val doubleEncodedLang = Cbor.encode(singleEncodedLang).toByteArray
        val singleEncodedCostModel = Cbor.encode(costModelArray).toByteArray
        val doubleEncodedCostModel = Cbor.encode(singleEncodedCostModel).toByteArray
        val expected = doubleEncodedLang ++ doubleEncodedCostModel
        
        assert(encoding.sameElements(expected))
        
        // Print hex for debugging
        println(s"PlutusV1 encoding: ${encoding.toHex}")
        println(s"Expected encoding: ${expected.toHex}")
    }

    test("getLanguageViewEncoding should handle PlutusV2 with standard encoding") {
        // Test PlutusV2 cost model encoding with standard encoding
        val plutusV2CostModel = IndexedSeq(10L, 20L, 30L, 40L, 50L)
        val costModels = CostModels(Map(1 -> plutusV2CostModel))
        
        val encoding = costModels.getLanguageViewEncoding
        
        // Verify the encoding is not empty
        assert(encoding.nonEmpty)
        
        // Manually construct expected encoding for verification
        val langId = 1
        val costModelArray = plutusV2CostModel.map(_.toInt).toArray
        
        // PlutusV2: Standard encoding
        val encodedLang = Cbor.encode(langId).toByteArray
        val encodedCostModel = Cbor.encode(costModelArray).toByteArray
        val expected = encodedLang ++ encodedCostModel
        
        assert(encoding.sameElements(expected))
        
        // Print hex for debugging
        println(s"PlutusV2 encoding: ${encoding.toHex}")
        println(s"Expected encoding: ${expected.toHex}")
    }

    test("getLanguageViewEncoding should handle PlutusV3 with standard encoding") {
        // Test PlutusV3 cost model encoding with standard encoding
        val plutusV3CostModel = IndexedSeq(100L, 200L, 300L, 400L, 500L)
        val costModels = CostModels(Map(2 -> plutusV3CostModel))
        
        val encoding = costModels.getLanguageViewEncoding
        
        // Verify the encoding is not empty
        assert(encoding.nonEmpty)
        
        // Manually construct expected encoding for verification
        val langId = 2
        val costModelArray = plutusV3CostModel.map(_.toInt).toArray
        
        // PlutusV3: Standard encoding (same as V2)
        val encodedLang = Cbor.encode(langId).toByteArray
        val encodedCostModel = Cbor.encode(costModelArray).toByteArray
        val expected = encodedLang ++ encodedCostModel
        
        assert(encoding.sameElements(expected))
        
        // Print hex for debugging
        println(s"PlutusV3 encoding: ${encoding.toHex}")
        println(s"Expected encoding: ${expected.toHex}")
    }

    test("getLanguageViewEncoding should handle multiple languages in order") {
        // Test multiple languages - should be concatenated in sorted order
        val plutusV1CostModel = IndexedSeq(1L, 2L, 3L)
        val plutusV2CostModel = IndexedSeq(10L, 20L, 30L)
        val plutusV3CostModel = IndexedSeq(100L, 200L, 300L)
        
        val costModels = CostModels(Map(
            0 -> plutusV1CostModel,
            1 -> plutusV2CostModel, 
            2 -> plutusV3CostModel
        ))
        
        val encoding = costModels.getLanguageViewEncoding
        
        // Manually construct expected encoding
        // V1 (double-bagged)
        val v1LangId = 0
        val v1Array = plutusV1CostModel.map(_.toInt).toArray
        val v1SingleLang = Cbor.encode(v1LangId).toByteArray
        val v1DoubleLang = Cbor.encode(v1SingleLang).toByteArray
        val v1SingleCost = Cbor.encode(v1Array).toByteArray
        val v1DoubleCost = Cbor.encode(v1SingleCost).toByteArray
        val v1Expected = v1DoubleLang ++ v1DoubleCost
        
        // V2 (standard)
        val v2LangId = 1
        val v2Array = plutusV2CostModel.map(_.toInt).toArray
        val v2Lang = Cbor.encode(v2LangId).toByteArray
        val v2Cost = Cbor.encode(v2Array).toByteArray
        val v2Expected = v2Lang ++ v2Cost
        
        // V3 (standard)
        val v3LangId = 2
        val v3Array = plutusV3CostModel.map(_.toInt).toArray
        val v3Lang = Cbor.encode(v3LangId).toByteArray
        val v3Cost = Cbor.encode(v3Array).toByteArray
        val v3Expected = v3Lang ++ v3Cost
        
        val expected = v1Expected ++ v2Expected ++ v3Expected
        
        assert(encoding.sameElements(expected))
        
        // Print hex for debugging
        println(s"Multi-language encoding: ${encoding.toHex}")
        println(s"Expected encoding: ${expected.toHex}")
    }

    test("getLanguageViewEncoding should handle empty cost model") {
        // Test with empty cost model for PlutusV2
        val costModels = CostModels(Map(1 -> IndexedSeq.empty[Long]))
        
        val encoding = costModels.getLanguageViewEncoding
        
        // Manually construct expected encoding
        val langId = 1
        val encodedLang = Cbor.encode(langId).toByteArray
        val encodedNull = Cbor.encode(null).toByteArray
        val expected = encodedLang ++ encodedNull
        
        assert(encoding.sameElements(expected))
        
        // Print hex for debugging
        println(s"Empty cost model encoding: ${encoding.toHex}")
        println(s"Expected encoding: ${expected.toHex}")
    }

    test("getLanguageViewEncoding should throw for unknown language ID") {
        // Test with invalid language ID
        val costModels = CostModels(Map(99 -> IndexedSeq(1L, 2L, 3L)))
        
        assertThrows[IllegalArgumentException] {
            costModels.getLanguageViewEncoding
        }
    }

    test("Script data hash generation with known golden value") {
        // Test the complete script data hash generation using our encoding
        val era = Era.Conway
        val redeemers = Seq.empty[Redeemer]
        val datums = KeepRaw(TaggedSet.empty[Data])
        val costModels = CostModels(Map.empty)
        
        val hash = ScriptDataHashGenerator.computeScriptDataHash(era, redeemers, datums, costModels)
        
        // This should match the golden test value from the existing test
        val expectedHash = "9eb0251b2e85b082c3706a3e79b4cf2a2e96f936e912a398591e2486c757f8c1"
        assert(hash.toHex == expectedHash)
        
        println(s"Script data hash: ${hash.toHex}")
        println(s"Expected hash: $expectedHash")
    }

    test("Language view encoding differences between V1 and V2") {
        // Compare V1 and V2 encoding of the same cost model to verify double-bagging
        val costModel = IndexedSeq(1L, 2L, 3L, 4L, 5L)
        
        val v1CostModels = CostModels(Map(0 -> costModel))
        val v2CostModels = CostModels(Map(1 -> costModel))
        
        val v1Encoding = v1CostModels.getLanguageViewEncoding
        val v2Encoding = v2CostModels.getLanguageViewEncoding
        
        // V1 should be longer due to double-bagging
        assert(v1Encoding.length > v2Encoding.length)
        
        // Verify they're different
        assert(!v1Encoding.sameElements(v2Encoding))
        
        println(s"V1 encoding length: ${v1Encoding.length}, hex: ${v1Encoding.toHex}")
        println(s"V2 encoding length: ${v2Encoding.length}, hex: ${v2Encoding.toHex}")
    }

    test("Language view encoding with realistic cost model values") {
        // Test with realistic cost model values similar to what would be used in mainnet
        val plutusV1RealCostModel = IndexedSeq(
            205665L, 812L, 1L, 1L, 1000L, 571L, 0L, 1L, 1000L, 24177L, 4L, 1L, 1000L, 32L,
            117366L, 10475L, 4L, 23000L, 100L, 23000L, 100L, 23000L, 100L, 23000L, 100L,
            23000L, 100L, 23000L, 100L, 100L, 100L, 23000L, 100L, 19537L, 32L, 175354L,
            32L, 46417L, 4L, 221973L, 511L, 0L, 1L, 89141L, 32L, 497525L, 14068L, 4L,
            2L, 196500L, 453240L, 220L, 0L, 1L, 1L, 1000L, 28662L, 4L, 2L, 245000L,
            216773L, 62L, 1L, 1060367L, 12586L, 1L, 208512L, 421L, 1L, 187000L, 1000L,
            52998L, 1L, 80436L, 32L, 43249L, 32L, 1000L, 32L, 80556L, 1L, 57667L, 4L,
            1000L, 10L, 197145L, 156L, 1L, 197145L, 156L, 1L, 204924L, 473L, 1L,
            208896L, 511L, 1L, 52467L, 32L, 64832L, 32L, 65493L, 32L, 22558L, 32L,
            16563L, 32L, 76511L, 32L, 196500L, 453240L, 220L, 0L, 1L, 1L, 69522L, 11687L,
            0L, 1L, 60091L, 32L, 196500L, 453240L, 220L, 0L, 1L, 1L, 196500L, 453240L,
            220L, 0L, 1L, 1L, 1159724L, 392670L, 0L, 2L, 806990L, 30482L, 4L, 1927926L,
            82523L, 4L, 265318L, 0L, 4L, 0L, 85931L, 32L, 205665L, 812L, 1L, 1L, 41182L,
            32L, 212342L, 32L, 31220L, 32L, 32696L, 32L, 43357L, 32L, 32247L, 32L,
            38314L, 32L, 9462713L, 1021L, 10L, 38887044L, 32947L, 10L
        )
        
        val costModels = CostModels(Map(0 -> plutusV1RealCostModel))
        val encoding = costModels.getLanguageViewEncoding
        
        // Verify encoding is not empty and has reasonable length
        assert(encoding.nonEmpty)
        assert(encoding.length > 100) // Should be substantial due to large cost model
        
        println(s"Realistic V1 cost model encoding length: ${encoding.length}")
        println(s"First 100 bytes: ${encoding.take(100).toHex}")
    }
}