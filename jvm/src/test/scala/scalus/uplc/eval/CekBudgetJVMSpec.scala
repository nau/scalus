package scalus.uplc
package eval

import cats.syntax.all.*
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.JVMPlatformSpecific
import scalus.flat.Flat
import scalus.ledger.api.BuiltinSemanticsVariant
import scalus.ledger.api.PlutusLedgerLanguage.*
import scalus.ledger.babbage.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Term.*
import scalus.uplc.eval.CekMachineCosts.defaultMachineCosts.*
import upickle.default.*

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
              MachineParams.defaultPlutusV2PostConwayParams,
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
        val input = this.getClass().getResourceAsStream("/protocol-params.json")
        val pparams = read[ProtocolParams](input)
        testReadingCostModelParams(pparams)
    }

    test("Plutus V3 SoP optimization evaluation") {
        import scalus.uplc.TermDSL.{*, given}
        import scalus.builtin.given
        import scalus.uplc.FlatInstantces.given
        given PlutusVM = PlutusVM.makePlutusV3VM()

        // generate (\f -> f 1 ... N) (\a1... aN -> a1)
        def genLamApplyTerm(n: Int): Term =
            val lamN = λ((1 to n).map(i => s"a$i")*)(vr"a1")
            val apply = (1 to n).foldLeft(vr"f")((acc, i) => acc $ i)
            λ("f")(apply) $ lamN
        // generate (\f -> (case (constr 0 [1 ... N]) f)) (\a1... aN -> a1)
        def genSopTerm(n: Int): Term =
            val lamN = λ((1 to n).map(i => s"a$i")*)(vr"a1")
            val args: List[Term] = (1 to n).map(i => i: Term).toList
            λ("f")(Case(Constr(0, args), List(vr"f"))) $ lamN
        def compareN(n: Int): Unit =
            val lamApply = genLamApplyTerm(n)
            val sop = genSopTerm(n)
            val termFlat = summon[Flat[Term]]
            val lamApplySize = termFlat.bitSize(lamApply)
            val sopSize = termFlat.bitSize(sop)
            println(
              s"n=$n, apply: ${lamApply.evaluateDebug.budget.showJson} $lamApplySize bits, " +
                  s"sop: ${sop.evaluateDebug.budget.showJson}, size=${sopSize}"
            )
            println(
              f"sop/apply: ${sopSize.toDouble / lamApplySize}%2.2f of size, " +
                  f"${sop.evaluateDebug.budget.cpu.toDouble / lamApply.evaluateDebug.budget.cpu}%2.2f of cpu, " +
                  f"${sop.evaluateDebug.budget.memory.toDouble / lamApply.evaluateDebug.budget.memory}%2.2f of memory"
            )
//        compareN(1) // apply is more efficient for n=1
//        compareN(2) // same efficiency for n=2
//        compareN(3) // sop is more efficient for n=3 and more
    }

    test("BuiltinCostModel JSON reader from Blockfrost Protocol Parameters") {
        val input = this.getClass().getResourceAsStream("/blockfrost-params-epoch-507.json")
        val pparams = read[ProtocolParams](input)(using ProtocolParams.blockfrostParamsRW)
        testReadingCostModelParams(pparams)
    }

    private def testReadingCostModelParams(pparams: ProtocolParams): Unit = {
        val v1 = pparams.costModels("PlutusV1")
        val v2 = pparams.costModels("PlutusV2")
        val v3 = pparams.costModels("PlutusV3")
        val paramsV1 = PlutusV1Params.fromSeq(v1)
        val paramsV2 = PlutusV2Params.fromSeq(v2)
        val paramsV3 = PlutusV3Params.fromSeq(v3)
        val paramsV1Map = writeJs(paramsV1).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val paramsV2Map = writeJs(paramsV2).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        val paramsV3Map = writeJs(paramsV3).obj.map { case (k, v) => (k, v.num.toLong) }.toMap
        // check that we can read the parameters
        BuiltinCostModel.fromCostModelParams(PlutusV1, BuiltinSemanticsVariant.B, paramsV1Map)
        BuiltinCostModel.fromCostModelParams(PlutusV2, BuiltinSemanticsVariant.B, paramsV2Map)
        BuiltinCostModel.fromCostModelParams(PlutusV3, BuiltinSemanticsVariant.C, paramsV3Map)
    }
