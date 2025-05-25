package scalus

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.{Builtins, ByteString}
import scalus.builtin.ToData
import scalus.builtin.ToData.toData
import scalus.uplc.*
import scalus.builtin.Data

import scala.annotation.nowarn

@Compile
object TotoDataInstances {
    given Data.ToData[BigInt] = (a: BigInt) => builtin.Builtins.iData(a)
    given Data.ToData[String] = (a: String) => builtin.Builtins.iData(1)
}

case class Test(a: BigInt, b: String)
@Compile
object Test {
    import TotoDataInstances.given
    given Data.ToData[Test] = ToData.deriveCaseClass[Test](12)
}

class CustomError extends Exception("custom error")

class CompilerPluginToSIRTest extends AnyFunSuite with ScalaCheckPropertyChecks:
    val deadbeef = Constant.ByteString(hex"deadbeef")

    test("compile literals") {
        val sir = compile {
            @nowarn
            def err(msg: String): Nothing = throw new RuntimeException(msg)
            err("test")
        }
        println(sir)
        println(sir.pretty.render(100))
        println(sir.toUplcOptimized().showHighlighted)
    }
