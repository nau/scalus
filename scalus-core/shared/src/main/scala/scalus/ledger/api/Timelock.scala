package scalus.ledger.api
import io.bullet.borer.*
import io.bullet.borer.derivation.ArrayBasedCodecs.*
import io.bullet.borer.derivation.key
import scalus.ledger.api.Timelock.{lteNegInfty, ltePosInfty}

import scala.annotation.tailrec

// SlotNo wrapped value
type SlotNo = Long

/** ValidityInterval is a half-open interval: closed on bottom, open on top.
  *
  * [[None]] on bottom is negative infinity, [[None]] on top is positive infinity
  */
case class ValidityInterval(
    invalidBefore: Option[SlotNo],
    invalidHereafter: Option[SlotNo]
)

/** Cardano Timelock script
  *
  * Timelock scripts are used to enforce time-based constraints on transactions. Implemented
  * according to "A Formal Specification of the Cardano Ledger with a Native Multi-Asset
  * Implementation" specification.
  *
  * @see
  *   [evalTimelock](https://github.com/IntersectMBO/cardano-ledger/blob/d428f5bfcf60c9e0d9503f097175e61c968fefb9/eras/allegra/impl/src/Cardano/Ledger/Allegra/Scripts.hs#L346)
  */
enum Timelock derives Codec.All:
    @key(0) case Signature(keyHash: KeyHash)
    @key(1) case AllOf(scripts: Seq[Timelock])
    @key(2) case AnyOf(scripts: Seq[Timelock])
    @key(3) case MOf(m: Int, scripts: Seq[Timelock])
    @key(4) case TimeStart(lockStart: SlotNo)
    @key(5) case TimeExpire(lockExpire: SlotNo)

    // String representation of Timelock
    lazy val show: String = this match
        case Timelock.TimeStart(i)  => s"(Start >= $i)"
        case Timelock.TimeExpire(i) => s"(Expire < $i)"
        case Timelock.AllOf(xs) =>
            val inner = xs.map(_.show).mkString(" ")
            s"(AllOf $inner)"
        case Timelock.AnyOf(xs) =>
            val inner = xs.map(_.show).mkString(" ")
            s"(AnyOf $inner)"
        case Timelock.MOf(m, xs) =>
            val inner = xs.map(_.show).mkString(" ")
            s"(MOf $m $inner)"
        case Timelock.Signature(hash) => s"(Signature $hash)"

    /** Evaluates a Timelock script against a set of validator keys and a validity interval
      *
      * @param validatorKeys
      *   Set of validator key hashes
      * @param interval
      *   The transaction validity interval
      * @param script
      *   The timelock script to evaluate
      * @return
      *   true if the script evaluates to true, false otherwise
      */
    def evaluate(
        validatorKeys: Set[KeyHash],
        interval: ValidityInterval
    ): Boolean = {
        this match
            case Timelock.Signature(keyHash) =>
                validatorKeys.contains(keyHash)

            case Timelock.AllOf(scripts) =>
                scripts.forall(s => s.evaluate(validatorKeys, interval))

            case Timelock.AnyOf(scripts) =>
                scripts.exists(s => s.evaluate(validatorKeys, interval))

            case Timelock.MOf(m, scripts) =>
                // Using a recursive approach to validate M of N scripts
                @tailrec
                def isValidMOf(n: Int, remaining: Seq[Timelock]): Boolean =
                    if n <= 0 then true
                    else
                        remaining match
                            case Seq() => false
                            case Seq(script, tail*) =>
                                if script.evaluate(validatorKeys, interval) then
                                    isValidMOf(n - 1, tail)
                                else isValidMOf(n, tail)

                isValidMOf(m, scripts)

            case Timelock.TimeStart(lockStart) =>
                lteNegInfty(lockStart, interval.invalidBefore)

            case Timelock.TimeExpire(lockExpire) =>
                ltePosInfty(interval.invalidHereafter, lockExpire)
    }

    def toCbor: Array[Byte] = Cbor.encode(this).toByteArray

// Companion object with factory methods
object Timelock:
    /** Reads a Timelock script from a CBOR encoded byte array
      *
      * @param cbor
      *   The CBOR encoded byte array
      * @return
      *   The Timelock script
      */
    def fromCbor(cbor: Array[Byte]): Timelock =
        Cbor.decode(cbor).to[Timelock].value

    // Helper method to check if a slot is in a validity interval
    def inInterval(slot: SlotNo, interval: ValidityInterval): Boolean =
        (interval.invalidBefore, interval.invalidHereafter) match
            case (None, None)              => true
            case (None, Some(top))         => slot < top
            case (Some(bottom), None)      => bottom <= slot
            case (Some(bottom), Some(top)) => bottom <= slot && slot < top

    /** Checks if less-than-equal comparison holds where Nothing is negative infinity
      * @return
      *   true if i <= j, false if i > j or j is SNothing
      */
    def lteNegInfty(i: SlotNo, j: Option[SlotNo]): Boolean = j match
        case None    => false // i > -∞
        case Some(j) => i <= j

    /** Checks if less-than-equal comparison holds where Nothing is positive infinity
      * @return
      *   true if i <= j, false if i > j or i is SNothing
      */
    def ltePosInfty(i: Option[SlotNo], j: SlotNo): Boolean = i match
        case None    => false // ∞ > j
        case Some(i) => i <= j
