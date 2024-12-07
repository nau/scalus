package scalus.sir

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite

import scalus.flat.*

class SirToExprSpec extends AnyFunSuite {

    /*
    test("serialize/deserialize List") {
        val sir: SIRExpr = SIR.Const(
            scalus.uplc.Constant.List(
                scalus.uplc.DefaultUni.Integer,
                List(
                    scalus.uplc.Constant.Integer(1),
                    scalus.uplc.Constant.Integer(2),
                    scalus.uplc.Constant.Integer(3)
                )),
            SIRType.List(SIRType.IntegerPrimitive)
        )
        println(s"sir:${sir}")
        val len = ToExprHSSIRFlat.bitSize(sir)
        println(s"List bit size: $len")
        val encoded = EncoderState(len / 8 + 1)
        ToExprHSSIRFlat.encode(sir, encoded)
        val decoded = ToExprHSSIRFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRExpr]
        assert(sir ~=~ decoded)
    }



    test("serialize/deserialize ListType") {
        val tp = SIRType.List(SIRType.IntegerPrimitive)
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        //println(s"ListType bit size: $bitSize")
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(tp ~=~ decoded)
    }

    test("list type fron macro is serialized without unfilled proxies") {
        val tp = SIRTypeMacros.liftM[scalus.builtin.List[Int]]
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        //println(s"ListType bit size: $bitSize")
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(SIRType.checkAllProxiesFilled(decoded))
        assert(tp ~=~ decoded)
    }

    test("list type of buildin fun is serialized without unfilled proxies") {
        val fun = SIRBuiltins.tailList
        val tp = fun.tp
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        //println(s"ListType bit size: $bitSize")
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(SIRType.checkAllProxiesFilled(decoded))
        println("decoded tyoe: " + decoded)
        assert(tp ~=~ decoded)
    }
    */

    test("type of buildin fun application is serialized without unfilled proxies") {
        val fun = SIRBuiltins.tailList
        val arg: SIR = SIR.Const(scalus.uplc.Constant.List(scalus.uplc.DefaultUni.Integer,
                                                                List(scalus.uplc.Constant.Integer(1))),
                                      SIRType.List(SIRType.IntegerPrimitive))
        val fun1 = SIR.Apply(fun, arg, SIRType.calculateApplyType(fun.tp, arg.tp, Map.empty))
        val tp = fun1.tp
        val bitSize = ToExprHSSIRTypeFlat.bitSize(tp)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRTypeFlat.encode(tp, encoded)
        val decoded = ToExprHSSIRTypeFlat.decode(DecoderState(encoded.buffer)).asInstanceOf[SIRType]
        assert(SIRType.checkAllProxiesFilled(decoded))
        assert(tp ~=~ decoded)
    }



    test("buildin fun application is serialized without unfilled proxies") {
        val fun0 = SIRBuiltins.tailList
        val arg: SIR = SIR.Const(scalus.uplc.Constant.List(scalus.uplc.DefaultUni.Integer,
            List(scalus.uplc.Constant.Integer(1))),
            SIRType.List(SIRType.IntegerPrimitive))
        val fun1 = SIR.Apply(fun0, arg, SIRType.calculateApplyType(fun0.tp, arg.tp, Map.empty))
        println(s"fun1: ${fun1}")
        val bitSize = ToExprHSSIRFlat.bitSize(fun1)
        val encoded = EncoderState(bitSize / 8 + 1)
        ToExprHSSIRFlat.encode(fun1, encoded)
        val decoded = ToExprHSSIRFlat.decode(DecoderState(encoded.buffer))
        println(s"decoded: ${decoded}")
        assert(fun1 ~=~ decoded)
    }



}