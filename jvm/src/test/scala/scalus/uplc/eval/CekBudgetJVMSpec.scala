package scalus.uplc
package eval

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.given
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*

class CekBudgetJVMSpec extends AnyFunSuite:
    test("Check machine budget for terms is correct") {
        import cats.syntax.all.*
        import Cek.defaultMachineCosts.*
        def check(term: Term, expected: ExBudget) = {
            val cek = CekMachine(Cek.defaultEvaluationContext)
            val res = PlutusUplcEval.evalFlat(Program((1, 0, 0), term))
            (cek.runCek(term), res) match
                case (CekResult.Success(t1, budget), UplcEvalResult.Success(t2, budget2)) =>
                    assert(budget == (expected |+| Cek.defaultMachineCosts.startupCost))
                    assert(budget == budget2)
                case r => fail(s"Unexpected result for term $term: $r")
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
    }

    test("Check builtinCostModel.json == defaultBuiltinCostModel") {
        val input = this.getClass().getResourceAsStream("/builtinCostModel.json")
        val costModel = BuiltinCostModel.fromInputStream(input)
        assert(costModel == BuiltinCostModel.defaultBuiltinCostModel)
    }

    ignore("Check jvm/src/main/resources/cekMachineCosts.json.json == defaultMachineCosts") {
        val input = this.getClass().getResourceAsStream("/cekMachineCosts.json")
        val costModel = BuiltinCostModel.fromInputStream(input)
        assert(costModel == BuiltinCostModel.defaultBuiltinCostModel)
    }

    test("Run") {
        // val program = compile(()).toPlutusProgram((1, 0, 0), false)
        val program = compile(Builtins.sha2_256(ByteString.empty)).toPlutusProgram((1, 0, 0), false)
        // val program = compile(Builtins.addInteger(1, 2)).toPlutusProgram((1, 0, 0), false)
        println(program.pretty.render(80))
        val res = PlutusUplcEval.evalFlat(program)
        println(res)

        val r = CostingFun(
          OneArgument.LinearCost(OneVariableLinearFunction(806990, 30482)),
          OneArgument.ConstantCost(4)
        ).calculateCost(
          Seq(VCon(Constant.Integer(3)))
        )
        println(r)
        val cek = CekMachine(Cek.defaultEvaluationContext)
        cek.evalUPLC(program.term)
        println(s"${cek.getExBudget}")
    }
