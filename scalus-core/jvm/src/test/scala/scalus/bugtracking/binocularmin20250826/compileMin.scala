package scalus.bugtracking.binocularmin20250826

import scalus.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data.{toData, FromData, ToData}
import scalus.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.uplc.Program

import org.scalatest.funsuite.AnyFunSuite

class CompileMin extends AnyFunSuite {

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("compile call of byteStringToInteger to UPLC") {
        // val sir = Compiler.compile(BitcoinValidator.validate)
        val sir = Compiler.compile {

            // val powLimit: BigInt =
            byteStringToInteger(
              true,
              hex"00000000ffffffffffffffffffffffffffffffffffffffffffffffffffffffff"
            )
            // powLimit
        }
        //    println(sir.showHighlighted)
        sir.toUplcOptimized().plutusV3
    }

}
