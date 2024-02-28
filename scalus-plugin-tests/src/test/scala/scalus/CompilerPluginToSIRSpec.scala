package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtin.ByteString.given
import scalus.builtin.{Builtins, ByteString}
import scalus.builtin.ToData
import scalus.builtin.ToData.toData
import scalus.uplc.*

@Compile
object TotoDataInstances {
    given ToData.ToData[BigInt] = (a: BigInt) => builtin.Builtins.mkI(a)
    given ToData.ToData[String] = (a: String) => builtin.Builtins.mkI(1)
}

case class Test(a: BigInt, b: String)
@Compile
object Test {
    import TotoDataInstances.given
    given ToData.ToData[Test] = ToData.deriveCaseClass[Test](12)
}

class CompilerPluginToSIRSpec extends AnyFunSuite with ScalaCheckPropertyChecks:
    val deadbeef = Constant.ByteString(hex"deadbeef")

    test("compile literals") {
        val sir = compile {
            val a = new Test(1, "asdf").toData
            a
        }
        println(sir.pretty.render(100))
    }
