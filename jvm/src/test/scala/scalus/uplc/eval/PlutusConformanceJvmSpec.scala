package scalus
package uplc
package eval

import scala.io.Source.fromFile
import scala.language.implicitConversions
import scala.util.Using

/** Tests for the Plutus Conformance Test Suite.
  *
  * @note
  *   This tests run only on JVM right now.
  */
class PlutusConformanceJvmSpec extends PlutusConformanceSpec:
    override protected val path = s"../${super.path}"
    protected def readFile(path: String): String = {
        Using.resource(scala.io.Source.fromFile(path))(_.mkString)
    }
