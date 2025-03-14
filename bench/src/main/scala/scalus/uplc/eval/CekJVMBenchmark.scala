package scalus.uplc.eval

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import scalus.*
import scalus.uplc.{DeBruijnedProgram, DefaultFun, Program, Term}

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

@State(Scope.Benchmark)
class CekJVMBenchmark:
    @Param(
      Array(
        "auction_1-1.flat",
        "auction_1-2.flat",
        "auction_1-3.flat",
        "auction_1-4.flat"
      )
    )
    private var file: String = ""
    private var path: Path = null
    private val vm = PlutusVM.makePlutusV2VM()

    @Setup
    def readProgram() = {
        path = Paths.get(s"src/main/resources/data/$file")
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def bench() = {
        val bytes = Files.readAllBytes(path)
        val program = DeBruijnedProgram.fromFlatEncoded(bytes)
        vm.evaluateScript(program, RestrictingBudgetSpender(ExBudget.enormous), NoLogger)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def bench2() = {
        val bytes = Files.readAllBytes(path)
        val program = DeBruijnedProgram.fromFlatEncoded(bytes)
        vm.evaluateScript(program, RestrictingBudgetSpender(ExBudget.enormous), NoLogger)
    }

@State(Scope.Benchmark)
class JITBenchmark:
    private var program: DeBruijnedProgram = null
    private var program2: Program = null
    private val jitted = Test.getJitted()
    private val params = MachineParams.defaultPlutusV2PostConwayParams
    private val vm = PlutusVM.makePlutusV2VM(params)

    @Setup
    def readProgram() = {
        val path = Paths.get(s"src/main/resources/data/auction_1-1.flat")
        val bytes = Files.readAllBytes(path)
        program = DeBruijnedProgram.fromFlatEncoded(bytes)
//        program = DeBruijnedProgram
//            .fromDoubleCborHex(
//              "5901b55901b2010000333323222233300400300200122232323232324994ccd5cd19b8f37246eb8018dd71aba100414a22c60026eb0d5d09aba2357446ae88d5d11aba2357446ae88d5d11aab9e37546ae84d55cf1baa0043006225333573466ebccd5ce2481087369672e686561640035742002006266ae712401067369676e6564004984c008d5d10009aba1357440026aae78dd5001919180111980100100091801119801001000a60145d8799f582036c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49581d61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6ff004c010e4d5363616c757320726f636b7321004c01afd8799fd8799f808080a140a1401a0002de75a140a1401a0002de7580a0d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f581d61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6ffa0a0d8799f58201e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9ffffd87a9fd8799fd8799f58201ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982ff00ffffff0001"
//            )
        program2 = program.toProgram

    }

//    @Benchmark
//    @BenchmarkMode(Array(Mode.AverageTime))
//    @OutputTimeUnit(TimeUnit.MICROSECONDS)
//    def bench() = {
//        vm.evaluateScript(program, RestrictingBudgetSpender(ExBudget.enormous), NoLogger)
//    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def benchJIT() = {
        jitted(NoLogger, NoBudgetSpender, params)
    }

object Test {
    val path = Paths.get(s"src/main/resources/data/auction_1-1.flat")
    val bytes = Files.readAllBytes(path)
    val program = DeBruijnedProgram.fromFlatEncoded(bytes).toProgram
    /*val program = DeBruijnedProgram
        .fromDoubleCborHex(
          "5901b55901b2010000333323222233300400300200122232323232324994ccd5cd19b8f37246eb8018dd71aba100414a22c60026eb0d5d09aba2357446ae88d5d11aba2357446ae88d5d11aab9e37546ae84d55cf1baa0043006225333573466ebccd5ce2481087369672e686561640035742002006266ae712401067369676e6564004984c008d5d10009aba1357440026aae78dd5001919180111980100100091801119801001000a60145d8799f582036c71edaf0affadf6dd9c5f8df3dc90ec0fc01bf9f8cf0f18268db24b2a3da49581d61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6ff004c010e4d5363616c757320726f636b7321004c01afd8799fd8799f808080a140a1401a0002de75a140a1401a0002de7580a0d8799fd8799fd87980d87a80ffd8799fd87b80d87a80ffff9f581d61822dde476439a526070f36d3d1667ad099b462c111cd85e089f5e7f6ffa0a0d8799f58201e0612fbd127baddfcd555706de96b46c4d4363ac78c73ab4dee6e6a7bf61fe9ffffd87a9fd8799fd8799f58201ab6879fc08345f51dc9571ac4f530bf8673e0d798758c470f9af6f98e2f3982ff00ffffff0001"
        )
        .toProgram*/
    def collect(t: Term): Set[DefaultFun] = t match
        case Term.Var(name)          => Set()
        case Term.LamAbs(name, term) => collect(term)
        case Term.Apply(f, arg)      => collect(f) ++ collect(arg)
        case Term.Force(term)        => collect(term)
        case Term.Delay(term)        => collect(term)
        case Term.Const(const)       => Set()
        case Term.Builtin(bn)        => Set(bn)
        case Term.Error              => Set()
        case Term.Constr(tag, args)  => args.flatMap(collect).toSet
        case Term.Case(arg, cases)   => collect(arg) ++ cases.flatMap(collect)
    val builtins = collect(program.term).toSeq.sorted

    def getJitted() = {
        val start = System.currentTimeMillis()
        val r = JIT.jitUplc(program.term)
        val end = System.currentTimeMillis()
        println(s"JIT completed in ${end - start} ms")
        r
    }

    @main def run() = {
        given PlutusVM = PlutusVM.makePlutusV2VM()
        println(builtins)
        println(program.evaluateDebug)
//        println(program.showHighlighted)
//        println(JIT.jitUplc(program.term)())
        val spender = CountingBudgetSpender()
        getJitted()(NoLogger, spender, MachineParams.defaultPlutusV2PostConwayParams)
        println(spender.getSpentBudget.showJson)
    }
}
