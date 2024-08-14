package scalus.bloxbean

import com.bloxbean.cardano.client.plutus.spec.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.uplc.*
import scalus.uplc.eval.MachineParams
import scalus.uplc.eval.VM

import java.math.BigInteger
import java.util

class ScalusScriptUtilsSpec extends AnyFunSuite:
    private val program = Program(
      (1, 0, 0),
      compile((one: Data, two: Data) => one.toBigInt + two.toBigInt).toUplc()
    ).doubleCborHex

    test("applyParamsToScript with PlutusData varargs") {
        val applied = ScalusScriptUtils.applyParamsToScript(
          program,
          BigIntPlutusData(BigInteger.ONE),
          BigIntPlutusData(BigInteger.TWO)
        )
        val script = Program.fromDoubleCborHex(applied).flatEncoded
        val result = VM.evaluateScriptCounting(MachineParams.defaultParams, script)
        assert(result.term == Term.Const(Constant.Integer(3)))
    }

    test("applyParamsToScript with ListPlutusData") {
        val params =
            ListPlutusData
                .builder()
                .plutusDataList(
                  java.util.List
                      .of(BigIntPlutusData(BigInteger.ONE), BigIntPlutusData(BigInteger.TWO))
                )
                .build();
        val applied = ScalusScriptUtils.applyParamsToScript(program, params)
        val script = Program.fromDoubleCborHex(applied).flatEncoded
        val result = VM.evaluateScriptCounting(MachineParams.defaultParams, script)
        assert(result.term == Term.Const(Constant.Integer(3)))
    }
