package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.utils.Hex.toHex

class CostModelsTest extends AnyFunSuite {

    test("getLanguageViewEncoding should handle empty cost models") {
        val costModels = CostModels(Map.empty)
        val encoding = costModels.getLanguageViewEncoding
        assert(encoding.toHex == "a0")
    }
}
