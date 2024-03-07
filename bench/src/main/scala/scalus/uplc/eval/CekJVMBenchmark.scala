package scalus.uplc.eval

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Param
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State
import scalus.*
import scalus.builtin.given
import scalus.uplc.DeBruijn
import scalus.uplc.DefaultFun.*
import scalus.uplc.DefaultUni.asConstant
import scalus.uplc.Program
import scalus.uplc.ProgramFlatCodec
import scalus.uplc.Term.*
import scalus.uplc.TermDSL.{*, given}
import scalus.uplc.eval.*
import scalus.utils.Utils

import java.nio.file.Files
import java.nio.file.Paths
import scala.io.Source.fromFile
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import java.util.concurrent.TimeUnit
import scalus.uplc.Meaning

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
    private var program: Program = null
    val cek = CekMachine(Cek.plutusV2Params)

    @Setup
    def readProgram() = {
        val bytes = Files.readAllBytes(Paths.get(s"src/main/resources/data/$file"))
        val prg = ProgramFlatCodec.decodeFlat(bytes)
        val namedTerm = DeBruijn.fromDeBruijnTerm(prg.term)
        program = Program((1, 0, 0), namedTerm)
    }

    @Benchmark
    @BenchmarkMode(Array(Mode.AverageTime))
    @OutputTimeUnit(TimeUnit.MICROSECONDS)
    def bench() = {
        cek.evaluateTerm(program.term)
    }
