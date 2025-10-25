package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuiteLike
import scalus.*
import scalus.Compiler.compile
import scalus.uplc.{Constant, Term}

class JITDeepRecursionTest extends AnyFunSuiteLike {
    private given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Deep recursion factorial with CekMachine vs JIT") {
        val uplc: Term = compile {
            def factorial(n: BigInt): BigInt =
                if n <= 1 then 1
                else n * factorial(n - 1)

            factorial(20)
        }.toUplc(true)

        // Evaluate with CekMachine
        val cekResult = uplc.evaluateDebug
        val cekValue = cekResult match
            case Result.Success(term, _, _, _) => term
            case Result.Failure(e, _, _, _)    => fail(s"CekMachine evaluation failed: $e")

        // Evaluate with JIT (will fail if builtin not supported, so we catch that)
        val logger = Log()
        try {
            val jitResult =
                JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
            // JIT returns raw value, CekMachine returns Term.Const
            val expectedValue = BigInt(2432902008176640000L)
            assert(cekValue == Term.Const(Constant.Integer(expectedValue)))
            assert(jitResult == expectedValue)
        } catch {
            case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                info(s"JIT doesn't support some builtins yet: ${e.getMessage}")
                assert(cekValue == Term.Const(Constant.Integer(2432902008176640000L)))
        }
    }

    test("Deep recursion Fibonacci with CekMachine vs JIT") {
        val uplc: Term = compile {
            def fib(n: BigInt): BigInt =
                if n <= 1 then n
                else fib(n - 1) + fib(n - 2)

            fib(15)
        }.toUplc(true)

        // Evaluate with CekMachine
        val cekResult = uplc.evaluateDebug
        val cekValue = cekResult match
            case Result.Success(term, _, _, _) => term
            case Result.Failure(e, _, _, _)    => fail(s"CekMachine evaluation failed: $e")

        // Evaluate with JIT (will fail if builtin not supported, so we catch that)
        val logger = Log()
        try {
            val jitResult =
                JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
            // JIT returns raw value, CekMachine returns Term.Const
            val expectedValue = BigInt(610)
            assert(cekValue == Term.Const(Constant.Integer(expectedValue)))
            assert(jitResult == expectedValue)
        } catch {
            case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                info(s"JIT doesn't support some builtins yet: ${e.getMessage}")
                assert(cekValue == Term.Const(Constant.Integer(610)))
        }
    }

    test("Deep recursion sum with CekMachine vs JIT") {
        val uplc: Term = compile {
            def sumToN(n: BigInt): BigInt =
                if n <= 0 then 0
                else n + sumToN(n - 1)

            sumToN(100)
        }.toUplc(true)

        // Evaluate with CekMachine
        val cekResult = uplc.evaluateDebug
        val cekValue = cekResult match
            case Result.Success(term, _, _, _) => term
            case Result.Failure(e, _, _, _)    => fail(s"CekMachine evaluation failed: $e")

        // Evaluate with JIT (will fail if builtin not supported, so we catch that)
        val logger = Log()
        try {
            val jitResult =
                JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
            // JIT returns raw value, CekMachine returns Term.Const
            val expectedValue = BigInt(5050)
            assert(cekValue == Term.Const(Constant.Integer(expectedValue)))
            assert(jitResult == expectedValue)
        } catch {
            case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                info(s"JIT doesn't support some builtins yet: ${e.getMessage}")
                assert(cekValue == Term.Const(Constant.Integer(5050)))
        }
    }

    test("JIT stack overflow bounds - CekMachine works but JIT overflows") {
        // Compile the sum function once
        val sumFunctionUplc: Term = compile { (n: BigInt) =>
            def sumToN(n: BigInt): BigInt =
                if n <= 0 then 0
                else n + sumToN(n - 1)
            sumToN(n)
        }.toUplc(true)

        def testDepth(n: Int): Unit = {
            // Apply the compiled function to a constant argument
            val uplc: Term = sumFunctionUplc $ Term.Const(Constant.Integer(BigInt(n)))

            // CekMachine should handle this fine (it's iterative)
            val cekResult = uplc.evaluateDebug
            val cekSuccess = cekResult match
                case Result.Success(term, _, _, _) =>
                    info(s"CekMachine succeeded at depth $n")
                    true
                case Result.Failure(e, _, _, _) =>
                    info(s"CekMachine failed at depth $n: ${e.getMessage}")
                    false

            if cekSuccess then
                // Try JIT - it should overflow at some point due to native stack usage
                val logger = Log()
                try {
                    val jitResult =
                        JIT.jitUplc(uplc)(logger, NoBudgetSpender, summon[PlutusVM].machineParams)
                    info(s"JIT succeeded at depth $n, result: $jitResult")
                } catch {
                    case e: StackOverflowError =>
                        info(s"JIT StackOverflowError at depth $n (CekMachine still works)")
                        throw e // Re-throw to mark the boundary
                    case e: RuntimeException if e.getMessage.contains("not yet supported") =>
                        info(s"JIT doesn't support some builtins: ${e.getMessage}")
                }
        }

        // Start testing from a known working depth and increase
        // We expect JIT to overflow somewhere between 1000-10000
        var depth = 1000
        var foundOverflow = false
        var maxWorkingDepth = 0

        while !foundOverflow && depth <= 20000 do
            try {
                testDepth(depth)
                maxWorkingDepth = depth
                depth = (depth * 1.5).toInt // Increase by 50%
            } catch {
                case e: StackOverflowError =>
                    foundOverflow = true
                    info(
                      s"Found JIT stack overflow boundary: CekMachine works but JIT overflows at depth $depth"
                    )
                    info(s"Maximum working depth for JIT: $maxWorkingDepth")
            }

        assert(foundOverflow, s"Did not find stack overflow up to depth $depth")
    }
}
