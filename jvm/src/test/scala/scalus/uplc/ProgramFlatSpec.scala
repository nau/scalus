package scalus.uplc

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.utils.Utils

class ProgramFlatSpec extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
  test("Program flat encoding/decoding is identical to Plutus") {
    forAll { (p: Program) =>
      val str = p.pretty.render(80)
      val bytes = Utils.uplcToFlat(str)
      val encoded = ProgramFlatCodec.encodeFlat(p)
      assert(Utils.bytesToHex(bytes) == Utils.bytesToHex(encoded))
    }
  }
}
