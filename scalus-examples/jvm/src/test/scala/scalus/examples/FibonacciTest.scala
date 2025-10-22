package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.*
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.testkit.ScalusTest
import scalus.uplc.*
import scalus.uplc.Term.asTerm
import scalus.uplc.eval.*
import scalus.uplc.transform.{CaseConstrApply, Inliner}

import scala.annotation.tailrec
import scala.language.implicitConversions

/** Fibonacci Lookup Implementation using Pre-packed ByteString
  *
  * This implementation stores pre-computed Fibonacci numbers as a ByteString where each number is
  * encoded as 3 bytes (big-endian). The lookup function slices the appropriate 3 bytes and converts
  * them to an integer.
  *
  * Based on Plutarch implementation by SeungheonOh
  */
class FibonacciTest extends AnyFunSuite with ScalusTest {
    test("fibonacci packed") {

        // Helper to generate Fibonacci sequence as ByteString
        // Each Fibonacci number is encoded as 3 bytes in big-endian format
        def fibSeqByteString(n: Int): ByteString = {
            @tailrec
            def fib(a: Int, b: Int, index: Int, acc: Array[Byte]): Array[Byte] =
                if index >= n then acc
                else
                    val offset = index * 3
                    acc(offset) = ((a >> 16) & 0xff).toByte
                    acc(offset + 1) = ((a >> 8) & 0xff).toByte
                    acc(offset + 2) = (a & 0xff).toByte
                    fib(b, a + b, index + 1, acc)

            ByteString.fromArray(fib(0, 1, 0, new Array[Byte](n * 3)))
        }

        // Pre-computed Fibonacci sequence up to fib(25)
        val packedFibonacci = fibSeqByteString(26)

        // Compile Scalus function to lookup Fibonacci number from packed ByteString
        val fib = compile: (packedFibonacci: ByteString) =>
            (x: BigInt) =>
                if x < BigInt(0) then x
                else byteStringToInteger(true, sliceByteString(x * 3, 3, packedFibonacci))

        // Apply the packed fibonacci ByteString
        val fibTerm = fib.toUplc() $ packedFibonacci.asTerm
        // Optimize the term
        val optFibTerm = fibTerm |> Inliner.apply |> CaseConstrApply.apply

        given PlutusVM = PlutusVM.makePlutusV3VM()
        // Test several Fibonacci numbers
        val testCases = Seq(
          (0, 0),
          (1, 1),
          (2, 1),
          (3, 2),
          (5, 5),
          (10, 55),
          (15, 610),
          (19, 4181),
          (20, 6765),
          (25, 75025)
        )

        testCases.foreach { case (n, expected) =>
            (optFibTerm $ n.asTerm).evaluateDebug match {
                case Result.Success(term, budget, _, _) =>
                    assert(term == asTerm(expected), s"fib($n) should be $expected")
                case Result.Failure(err, budget, _, _) =>
                    fail(s"Evaluation failed for fib($n): $err")
            }
        }
    }

    test("fibonacci unfold") {
        val sir = compile(Fib.fib(BigInt(100)))
        println(sir.showHighlighted)
        val uplc: Term = sir.toUplc() |> Inliner.apply
        println(s"\n${uplc.pretty.render(Int.MaxValue)}\n")
        println(uplc.plutusV3.cborByteString.size)
        val x = uplc.evaluate
        println(x)
        assert(s"$x" === "Const(Integer(354224848179261915075))")
    }
}

@Compile
object Fib {
    def fib(n: BigInt): BigInt =
        @tailrec def f(n: BigInt, x: BigInt, y: BigInt): BigInt =
            if n > 1 then f(n - 1, y, x + y) else y
        f(n, 0, 1)
}
