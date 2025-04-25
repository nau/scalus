package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite

import scalus.*
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.eval.{*, given}
//import scalus.builtin.given
import scalus.Compiler.{compile, compileDebug}
import scalus.testutil.SIRModules

enum DerivingSpec_AE2 derives ToData, FromData:
    case DS2_A extends DerivingSpec_AE2
    case DS2_B(b: BigInt) extends DerivingSpec_AE2
    case DS2_C(b: BigInt, bs: ByteString) extends DerivingSpec_AE2

object DerivingSpecScope {

    enum AE1 derives ToData, FromData:
        case A extends AE1
        case B(b: BigInt) extends AE1
        case C(b: BigInt, bs: ByteString) extends AE1

}

class DerivingSpec extends AnyFunSuite {

    import DerivingSpecScope.*

    import scalus.builtin.given
    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Compile To/From Data for AE1") {

        val sir = compileDebug { (d: Data) =>
            val a = summon[scalus.prelude.FromData[AE1]](d)
            a match
                case AE1.A        => BigInt(1)
                case AE1.B(b)     => BigInt(2)
                case AE1.C(b, bs) => BigInt(3)
        }

        println(s"sir: ${sir.pretty.render(1000)}")

        val uplc = sir.toUplc(generateErrorTraces = true)

        val ae1 = AE1.A
        val ae1Data = summon[ToData[AE1]](ae1)

        val program1 = uplc.plutusV3 $ ae1Data

        val result1 = program1.evalDebug

        // assert(result1.isSuccess)
        result1 match
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(1)))
            case Result.Failure(e, _, _, logs) =>
                e.printStackTrace()
                println(s"logs=${logs}")

                println("loading module")
                val module = SIRModules.load("scalus.prelude.DerivingSpecScope$.AE1$")

                module.defs.foreach { b =>
                    println(s"${b.name}:\n ${b.value.pretty.render(100)}")
                }

                fail(s"Expected success, but got failure, logs=$logs")

    }

}
