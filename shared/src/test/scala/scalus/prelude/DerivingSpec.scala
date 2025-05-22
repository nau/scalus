package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite

import scalus.*
import scalus.prelude.*
import scalus.uplc.*
import scalus.uplc.eval.{*, given}
import scalus.Compiler.compile
import scalus.testutil.SIRModules
import scalus.builtin.*

/*

TODO: enble after
  - compiler with fix of https://github.com/scala/scala3/issues/23043
  - implementing of automatic generation of 'derived' methods on SIR level.

enum DerivingSpec_AE2 derives ToData, FromData:
    case DS2_A extends DerivingSpec_AE2
    case DS2_B(b: BigInt) extends DerivingSpec_AE2
    case DS2_C(b: BigInt, bs: ByteString) extends DerivingSpec_AE2

 */

object DerivingSpecScope {

    /*
    enum AE1 derives ToData, FromData:
        case A extends AE1
        case B(b: BigInt) extends AE1
        case C(b: BigInt, bs: ByteString) extends AE1

     */

}

enum DerivingSpec_AE3 {
    case DS3_A extends DerivingSpec_AE3
    case DS3_B(b: BigInt) extends DerivingSpec_AE3
    case DS3_C(b: BigInt, bs: ByteString) extends DerivingSpec_AE3
}

@Compile
object DerivingSpec_AE3 {

    given ToData[DerivingSpec_AE3] = ToData.derived
    given FromData[DerivingSpec_AE3] = FromData.derived

}

class DerivingSpec extends AnyFunSuite {

    import DerivingSpecScope.*

    import scalus.builtin.given
    protected given PlutusVM = PlutusVM.makePlutusV3VM()

    /*
    test("Compile To/From Data for AE1") {

        val sir = compile { (d: Data) =>
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
    
     */

    test("Compile To/From Data for AE3") {

        val sir = Compiler.compile { (d: Data) =>
            val a = summon[FromData[DerivingSpec_AE3]](d)
            a match
                case DerivingSpec_AE3.DS3_A        => BigInt(1)
                case DerivingSpec_AE3.DS3_B(b)     => BigInt(2)
                case DerivingSpec_AE3.DS3_C(b, bs) => BigInt(3)
        }

        // println(s"sir: ${sir.pretty.render(1000)}")

        val uplc = sir.toUplc(generateErrorTraces = true)
        val ae3 = DerivingSpec_AE3.DS3_A
        val ae3Data = summon[ToData[DerivingSpec_AE3]](ae3)
        val program1 = uplc.plutusV3 $ ae3Data
        val result1 = program1.evalDebug
        // assert(result1.isSuccess)
        result1 match
            case Result.Success(term, _, _, _) =>
                assert(term == Term.Const(Constant.Integer(1)))
            case Result.Failure(e, _, _, logs) =>
                e.printStackTrace()
                println(s"logs=${logs}")

                // println("loading module")
                // val module = SIRModules.load("scalus.prelude.DerivingSpec_AE3$")
                // module.defs.foreach { b =>
                //    println(s"${b.name}:\n ${b.value.pretty.render(100)}")
                // }

                fail(s"Expected success, but got failure, logs=$logs")
    }

}
