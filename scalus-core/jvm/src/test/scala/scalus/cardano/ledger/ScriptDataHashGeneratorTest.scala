package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.Data
import scalus.utils.Hex.toHex

class ScriptDataHashGeneratorTest extends AnyFunSuite {
    test("ScriptDataHashGenerator should generate correct hash for empty inputs") {
        val era = Era.Conway
        val redeemers = Seq.empty[Redeemer]
        val datums = Seq.empty[Data]
        val costModels = CostModels(Map.empty)
        val hash = ScriptDataHashGenerator.generate(era, redeemers, datums, costModels)
        assert(hash.toHex == "0a377cede1b50aedd9cbfb13e400ed7406058958399db7ae6a1db578b69a769d")
    }

    // Additional tests can be added here to cover more scenarios
}
