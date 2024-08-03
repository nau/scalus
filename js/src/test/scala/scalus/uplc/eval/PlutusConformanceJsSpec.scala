package scalus.uplc.eval

/** Tests for the Plutus Conformance Test Suite.
  *
  * @note
  *   This tests run only on JVM right now.
  */
class PlutusConformanceJsSpec extends PlutusConformanceSpec:
    override protected def readFile(path: String): String = {
        import scalajs.js.Dynamic.global as g
        val fs = g.require("fs")
        fs.readFileSync(path).toString
    }
