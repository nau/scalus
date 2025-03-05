package scalus.uplc.eval

import scala.util.Using

/** Tests for the Plutus Conformance Test Suite.
  *
  * @note
  *   This tests run only on JVM right now.
  */
class PlutusConformanceNativeSpec extends PlutusConformanceSpec {
    protected def readFile(path: String): String = {
        Using.resource(scala.io.Source.fromFile(path))(_.mkString)
    }
}
