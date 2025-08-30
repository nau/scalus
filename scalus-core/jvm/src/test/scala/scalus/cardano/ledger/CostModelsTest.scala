package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.toHex

class CostModelsTest extends AnyFunSuite {

    test("getLanguageViewEncoding should handle empty cost models") {
        val costModels = CostModels(Map.empty)
        val encoding = costModels.getLanguageViewEncoding
        assert(encoding.toHex == "a0")
    }

    test("PlutusV1 model should be serialized last when mixed with other versions") {
        // Test the ordering logic specifically mentioned in the LanguageViewEncoder comments
        val costModels = CostModels(
          Map(
            0 -> IndexedSeq(1L), // PlutusV1 - should be "greater" despite ID 0
            1 -> IndexedSeq(2L), // PlutusV2
            2 -> IndexedSeq(3L) // PlutusV3
          )
        )

        val encoding = costModels.getLanguageViewEncoding
        val hex = encoding.toHex

        // Verify the "most stupid part" mentioned in comments:
        // PlutusV1's double-encoded key makes it "bigger" and come last
        val plutusV1Key = "4100" // 41 00 = bytes(1), 00
        val plutusV2Key = "01" // 01 = uint(1)
        val plutusV3Key = "02" // 02 = uint(2)

        assert(hex == "a30181020281034100439f01ff")
    }
}
