package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtins.ByteString.given
import scalus.builtins.{Builtins, ByteString}
import scalus.uplc.*
import scalus.uplc.ToData.toData

@Compile
object TotoDataInstances {
    given ToData.ToData[BigInt] = (a: BigInt) => builtins.Builtins.mkI(a)
    given ToData.ToData[String] = (a: String) => builtins.Builtins.mkI(1)
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
