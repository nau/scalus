package scalus.uplc.eval

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import scalus.*
import scalus.uplc.ArbitraryInstances

class MemoryUsageTest extends AnyFunSuite with ScalaCheckPropertyChecks with ArbitraryInstances {
    test("memoryUsageInteger(0) == 1") {
        assert(MemoryUsage.memoryUsageInteger(0) == 1)
        assert(MemoryUsage.memoryUsageInteger2(0) == 1)
    }

    test("memoryUsageInteger(2^64) == 1") {
        assert(MemoryUsage.memoryUsageInteger(BigInt(2).pow(64) - 1) == 1)
        assert(MemoryUsage.memoryUsageInteger2(BigInt(2).pow(64) - 1) == 1)
        assert(MemoryUsage.memoryUsageInteger(BigInt(2).pow(64)) == 2)
        assert(MemoryUsage.memoryUsageInteger2(BigInt(2).pow(64)) == 2)
        assert(MemoryUsage.memoryUsageInteger(BigInt(2).pow(64) + 1) == 2)
        assert(MemoryUsage.memoryUsageInteger2(BigInt(2).pow(64) + 1) == 2)
    }

    test("MemoryUsage.memoryUsageInteger(x) == MemoryUsage.memoryUsageInteger2(x)") {
        forAll { (x: BigInt) =>
            assert(MemoryUsage.memoryUsageInteger(x) == MemoryUsage.memoryUsageInteger2(x))
            assert(MemoryUsage.memoryUsageInteger(-x) == MemoryUsage.memoryUsageInteger2(-x))
        }
    }
}
