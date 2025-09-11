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
import scalus.uplc.DeBruijnedProgram

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
