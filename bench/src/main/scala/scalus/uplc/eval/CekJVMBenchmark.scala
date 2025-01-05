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
import scalus.builtin.given
import scalus.uplc.DeBruijnedProgram

import java.nio.file.Files
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
    private var program: DeBruijnedProgram = null
    private val vm = PlutusVM.makePlutusV2VM()

    @Setup
    def readProgram() = {
        val bytes = Files.readAllBytes(Paths.get(s"src/main/resources/data/$file"))
        program = DeBruijnedProgram.fromFlatEncoded(bytes)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def bench() = {
        vm.evaluateScript(program, RestrictingBudgetSpender(ExBudget.enormous), NoLogger)
    }
