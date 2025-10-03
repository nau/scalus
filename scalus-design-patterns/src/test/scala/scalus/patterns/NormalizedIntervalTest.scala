package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import scalus.prelude.*
import scalus.ledger.api.v1.*
import scalus.cardano.onchain.OnchainError

class NormalizedIntervalTest extends AnyFunSuite {

    test("tryNormalize should return ClosedRange for finite bounds") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.Finite(100), true),
          to = IntervalBound(IntervalBoundType.Finite(200), true)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.ClosedRange(100, 200)))
    }

    test("tryNormalize should handle exclusive bounds for ClosedRange") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.Finite(10), false),
          to = IntervalBound(IntervalBoundType.Finite(20), false)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.ClosedRange(11, 19)))
    }

    test("tryNormalize should return FromNegInf for negative infinity lower bound") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.NegInf, false),
          to = IntervalBound(IntervalBoundType.Finite(150), true)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.FromNegInf(150)))
    }

    test("tryNormalize should handle exclusive upper bound for FromNegInf") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.NegInf, false),
          to = IntervalBound(IntervalBoundType.Finite(150), false)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.FromNegInf(149)))
    }

    test("tryNormalize should return ToPosInf for positive infinity upper bound") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.Finite(50), true),
          to = IntervalBound(IntervalBoundType.PosInf, false)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.ToPosInf(50)))
    }

    test("tryNormalize should handle exclusive lower bound for ToPosInf") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.Finite(50), false),
          to = IntervalBound(IntervalBoundType.PosInf, false)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.ToPosInf(51)))
    }

    test("tryNormalize should return Always for infinite bounds") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.NegInf, false),
          to = IntervalBound(IntervalBoundType.PosInf, false)
        )
        assert(interval.tryNormalize === Option.Some(NormalizedInterval.Always))
    }

    test("tryNormalize should return None for improper intervals") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.Finite(200), true),
          to = IntervalBound(IntervalBoundType.Finite(100), true)
        )
        assert(interval.tryNormalize === Option.None)
    }

    test("tryNormalize should return None for NegInf to NegInf") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.NegInf, false),
          to = IntervalBound(IntervalBoundType.NegInf, false)
        )
        assert(interval.tryNormalize === Option.None)
    }

    test("tryNormalize should return None for PosInf lower bound") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.PosInf, false),
          to = IntervalBound(IntervalBoundType.Finite(100), true)
        )
        assert(interval.tryNormalize === Option.None)
    }

    test("normalize should return NormalizedInterval for proper intervals") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.Finite(75), false),
          to = IntervalBound(IntervalBoundType.PosInf, false)
        )
        assert(interval.normalize === NormalizedInterval.ToPosInf(76))
    }

    test("normalize should throw OnchainError for improper intervals") {
        val interval = Interval(
          from = IntervalBound(IntervalBoundType.PosInf, false),
          to = IntervalBound(IntervalBoundType.Finite(100), true)
        )
        assertThrows[OnchainError] {
            interval.normalize
        }
    }

    test("Eq instance should work correctly for ClosedRange") {
        val interval1 = NormalizedInterval.ClosedRange(100, 200)
        val interval2 = NormalizedInterval.ClosedRange(100, 200)
        val interval3 = NormalizedInterval.ClosedRange(100, 300)

        assert(interval1 === interval2)
        assert(interval1 !== interval3)
    }

    test("Eq instance should work correctly for different variants") {
        val closedRange = NormalizedInterval.ClosedRange(100, 200)
        val fromNegInf = NormalizedInterval.FromNegInf(200)
        val toPosInf = NormalizedInterval.ToPosInf(100)
        val always = NormalizedInterval.Always

        assert(closedRange !== fromNegInf)
        assert(fromNegInf !== toPosInf)
        assert(toPosInf !== always)
        assert(always !== closedRange)
    }

    test("Ord instance should order variants correctly") {
        val closedRange = NormalizedInterval.ClosedRange(100, 200)
        val fromNegInf = NormalizedInterval.FromNegInf(200)
        val toPosInf = NormalizedInterval.ToPosInf(100)
        val always = NormalizedInterval.Always

        assert((closedRange <=> fromNegInf) === Order.Less)
        assert((fromNegInf <=> toPosInf) === Order.Less)
        assert((toPosInf <=> always) === Order.Less)
        assert((always <=> closedRange) === Order.Greater)
    }

    test("Ord instance should compare fields within same variant") {
        val range1 = NormalizedInterval.ClosedRange(100, 200)
        val range2 = NormalizedInterval.ClosedRange(100, 300)
        val range3 = NormalizedInterval.ClosedRange(200, 200)

        assert((range1 <=> range2) === Order.Less)
        assert((range1 <=> range3) === Order.Less)
        assert((range2 <=> range1) === Order.Greater)
    }

    test("Show instance should format ClosedRange correctly") {
        val interval = NormalizedInterval.ClosedRange(100, 200)
        assert(interval.show === "NormalizedInterval.ClosedRange(100, 200)")
    }

    test("Show instance should format FromNegInf correctly") {
        val interval = NormalizedInterval.FromNegInf(150)
        assert(interval.show === "NormalizedInterval.FromNegInf(150)")
    }

    test("Show instance should format ToPosInf correctly") {
        val interval = NormalizedInterval.ToPosInf(50)
        assert(interval.show === "NormalizedInterval.ToPosInf(50)")
    }

    test("Show instance should format Always correctly") {
        val interval = NormalizedInterval.Always
        assert(interval.show === "NormalizedInterval.Always")
    }
}
