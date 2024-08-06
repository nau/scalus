package scalus.uplc
package eval

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.JVMPlatformSpecific
import scalus.ledger.babbage.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.eval.CekMachineCosts.defaultMachineCosts.*

import scala.util.Success
import scala.util.Try

class CekBudgetJVMSpec extends AnyFunSuite:
    private def check(term: Term, expected: ExBudget): Unit = {
        val expectedBudget = expected |+| startupCost
        test(
          s"Check machine budget for terms ${term.pretty.flatten.render(100)} is $expectedBudget"
        ) {
            val debruijnedTerm = DeBruijn.deBruijnTerm(term)
            val spender = CountingBudgetSpender()
            val cek = CekMachine(
              MachineParams.defaultParams,
              spender,
              NoLogger,
              JVMPlatformSpecific
            )
            val res = UplcCli.evalFlat(Program((1, 0, 0), term))
            (Try(cek.evaluateTerm(debruijnedTerm)), res) match
                case (Success(t1), UplcEvalResult.Success(t2, budget2)) =>
                    val budget = spender.getSpentBudget
                    assert(
                      budget == expectedBudget,
                      s"for term $term"
                    )
                    assert(budget == budget2, "Budgets should match with cardano uplc eval")
                case r => fail(s"Unexpected result for term $term: $r")
        }
    }

    val h = Const(asConstant("Hello"))
    val id = LamAbs("x", Var(NamedDeBruijn("x")))
    val app = Apply(id, h)
    val delay = Delay(h)
    val force = Force(delay)
    val builtin = Builtin(DefaultFun.MkNilData)

    check(h, constCost)
    check(id, lamCost)
    check(app, applyCost |+| lamCost |+| varCost |+| constCost)
    check(delay, delayCost)
    check(force, forceCost |+| delayCost |+| constCost)
    check(builtin, builtinCost)

    test("BuiltinCostModel JSON reader from Cardano Protocol Parameters") {
        import upickle.default.*
        val input = this.getClass().getResourceAsStream("/protocol-params.json")
        val pparams = read[ProtocolParams](input)
        val v1 = pparams.costModels("PlutusV1")
        val v2 = pparams.costModels("PlutusV2")
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV1Map = writeJs(paramsV1).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val paramsV2Map = writeJs(paramsV2).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val modelV1 = BuiltinCostModel.fromCostModelParams(paramsV1Map)
        val modelV2 = BuiltinCostModel.fromCostModelParams(paramsV2Map)
        assert(
          modelV1.flattenCostModel.keySet == BuiltinCostModel.defaultCostModel.flattenCostModel.keySet
        )
        assert(
          modelV2.flattenCostModel.keySet == BuiltinCostModel.defaultCostModel.flattenCostModel.keySet
        )
    }

    test("BuiltinCostModel JSON reader from Blockfrost Protocol Parameters") {
        import upickle.default.*
        val input = this.getClass().getResourceAsStream("/blockfrost-params-epoch-471.json")
        val pparams = read[ProtocolParams](input)(using ProtocolParams.blockfrostParamsRW)
        val v1 = pparams.costModels("PlutusV1")
        val v2 = pparams.costModels("PlutusV2")
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV1Map = writeJs(paramsV1).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val paramsV2Map = writeJs(paramsV2).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val modelV1 = BuiltinCostModel.fromCostModelParams(paramsV1Map)
        val modelV2 = BuiltinCostModel.fromCostModelParams(paramsV2Map)
        assert(
          modelV1.flattenCostModel.keySet == BuiltinCostModel.defaultCostModel.flattenCostModel.keySet
        )
        assert(
          modelV2.flattenCostModel.keySet == BuiltinCostModel.defaultCostModel.flattenCostModel.keySet
        )
    }
