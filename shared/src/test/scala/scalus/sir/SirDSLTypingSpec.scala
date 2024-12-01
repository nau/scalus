package scalus.sir

import scala.language.implicitConversions
import scalus.sir.SirDSL.{*, given}
import org.scalatest.funsuite.AnyFunSuite
import scalus.sir.SIR.LamAbs
import scalus.sir.SIRType.TypeVar

class SirDSLTypingSpec extends AnyFunSuite:

    test("type list of ints moved to SIR via implicit conversion") {
        val sir: SIR = List(1,2,3)
        
        assert(sir.tp == SIRType.List(SIRType.IntegerPrimitive))
        
    }

    test("list type is created without unfilled proxies") {
        val stp = SIRType.List(SIRType.IntegerPrimitive)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("pair type is created without unfilled proxies") {
        val stp = SIRType.Pair(SIRType.IntegerPrimitive, SIRType.BooleanPrimitive)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("function type is created without unfilled proxies") {
        val stp = SIRType.Fun(SIRType.IntegerPrimitive, SIRType.BooleanPrimitive)
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("list type fron macro is created without unfilled proxies") {
        val stp = SIRTypeMacros.liftM[scalus.builtin.List[Int]]
        assert(SIRType.checkAllProxiesFilled(stp))
    }

    test("tuple2 type from macro should be mapped to Pair") {
        val stp = SIRTypeMacros.liftM[(Int, Boolean)]
        assert(stp ~=~ SIRType.Pair(SIRType.IntegerPrimitive, SIRType.BooleanPrimitive))
    }

    test("fun from unit to tuple2 type should be mapped to FunPair") {
        val stp = SIRTypeMacros.liftM[ Unit => (Int, Boolean)]
        assert(stp ~=~ SIRType.Fun(SIRType.VoidPrimitive, SIRType.Pair(SIRType.IntegerPrimitive, SIRType.BooleanPrimitive)))
    }



    //test("calculate types extract pair types") {
    //    val pv = SIR.Var("p", (Data,Data))
    //
    //    val k = LamAbs(pv, SIRBuiltins.sndPair $ pv)
    //
    //    println(s"k.tp: ${k.tp.show } ")
    //
    //}
