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
    test("Budget") {
        val h = Const(asConstant("Hello"))
        val id = LamAbs("x", Var(NamedDeBruijn("x")))
        val app = Apply(id, h)
        val cek = CekMachine(Cek.defaultEvaluationContext)
        cek.evalUPLC(h)
        // assert(cek.evalUPLC(h) == h)
        println(s"${cek.getExBudget.cpu} ${cek.getExBudget.memory}")
    }

    test("Check builtinCostModel.json == defaultBuiltinCostModel") {
        import upickle.default.*
        val input = this.getClass().getResourceAsStream("/builtinCostModel.json")
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
