package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.utils.Utils

class ProgramFlatTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    ignore("Program flat encoding is identical to Plutus") {
        forAll { (p: Program) =>
            val str = p.show
            val bytes = UplcCli.uplcToFlat(str)
            assert(Utils.bytesToHex(bytes) == Utils.bytesToHex(p.flatEncoded))
        }
    }
}
