package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.utils.Utils

class ProgramFlatSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    test("Program flat encoding is identical to Plutus") {
        forAll { (p: Program) =>
            val str = p.pretty.render(80)
            val bytes = Utils.uplcToFlat(str)
            assert(Utils.bytesToHex(bytes) == Utils.bytesToHex(p.flatEncoded))
        }
    }
}
