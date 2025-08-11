package scalus.ledger.api.v1
import org.scalacheck.Arbitrary
import scalus.*
import scalus.prelude.StdlibTestKit

class IntervalTest extends StdlibTestKit with scalus.ledger.api.v1.ArbitraryInstances {

    test("`never` contains no values") {
        check { (time: PosixTime) =>
            !Interval.never.contains(time)
        }
        checkEval { (time: PosixTime) =>
            !Interval.never.contains(time)
        }
    }

    test("`always` contains all values") {
        check { (time: PosixTime) =>
            Interval.always.contains(time)
        }
        checkEval { (time: PosixTime) =>
            Interval.always.contains(time)
        }
    }

    test("point interval contains only its point") {
        forAll { (time: PosixTime) =>
            val interval = Interval.between(time, time)
            interval.contains(time) &&
            !interval.contains(time + 1) &&
            !interval.contains(time - 1)
        }
        checkEval { (time: PosixTime) =>
            val interval = Interval.between(time, time)
            interval.contains(time) &&
            !interval.contains(time + 1) &&
            !interval.contains(time - 1)
        }
    }

    test("interval contains values between its bounds") {
        forAll { (from: PosixTime, to: PosixTime) =>
            whenever(from < to) {
                val interval = Interval.between(from, to)
                interval.contains(from) &&
                interval.contains(to)
            }
        }
        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            interval.contains(100) && interval.contains(BigInt(200))
        }
    }

    test("entirelyAfter") {
        forAll { (from: PosixTime, to: PosixTime, testTime: PosixTime) =>
            whenever(from <= to) {
                val interval = Interval.between(from, to)

                if testTime < from then interval.entirelyAfter(testTime)
                else if testTime >= to then !interval.entirelyAfter(testTime)
                else interval.contains(testTime)
            }
        }
        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            interval.entirelyAfter(BigInt(50)) && !interval.entirelyAfter(BigInt(150)) && !interval
                .entirelyAfter(250)
        }
    }

    test("entirelyBefore") {
        forAll { (from: PosixTime, to: PosixTime, testTime: PosixTime) =>
            whenever(from <= to) {
                val interval = Interval.between(from, to)

                if testTime > to then interval.entirelyBefore(testTime)
                else if testTime <= from then !interval.entirelyBefore(testTime)
                else interval.contains(testTime)
            }
        }
        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            interval.entirelyBefore(250) && !interval.entirelyBefore(BigInt(150)) && !interval
                .entirelyBefore(BigInt(50))
        }
    }

    test("entirelyBetween") {
        forAll { (from: PosixTime, to: PosixTime, after: PosixTime, before: PosixTime) =>
            val orderedFrom = from.min(to)
            val orderedTo = from.max(to)
            val orderedAfter = after.min(before)
            val orderedBefore = after.max(before)

            val interval = Interval.between(orderedFrom, orderedTo)
            val entirelyBetween = interval.entirelyBetween(orderedAfter, orderedBefore)

            entirelyBetween == (interval.entirelyAfter(orderedAfter) && interval.entirelyBefore(
              orderedBefore
            ))
        }

        assertEval {
            val interval = Interval.between(BigInt(100), BigInt(200))
            val entirelyBetween = interval.entirelyBetween(BigInt(50), BigInt(250))
            entirelyBetween == (interval.entirelyAfter(BigInt(50)) && interval.entirelyBefore(
              BigInt(250)
            ))
        }
    }

    test("never interval is entirely after any time") {
        forAll { (time: PosixTime) =>
            !Interval.never.entirelyAfter(time)
        }
        checkEval { (time: PosixTime) =>
            !Interval.never.entirelyAfter(time)
        }
    }

    test("never interval is entirely before any time") {
        forAll { (time: PosixTime) =>
            !Interval.never.entirelyBefore(time)
        }
        checkEval { (time: PosixTime) =>
            !Interval.never.entirelyBefore(time)
        }
    }
}
