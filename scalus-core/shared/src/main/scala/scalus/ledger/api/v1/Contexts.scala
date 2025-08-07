package scalus.ledger.api.v1

import scalus.Compile
import scalus.builtin.{Builtins, ByteString, Data, FromData, ToData}
import scalus.builtin.Builtins.*
import scalus.prelude.{===, Eq, List, Option, Ord, given}
import scalus.prelude.Eq.given
import scalus.prelude.Ord.{<=>, ifEqualThen, given}
import scalus.builtin.ByteString.*
import scalus.ledger.api.v1.IntervalBoundType.{Finite, NegInf, PosInf}

type Hash = ByteString
type ValidatorHash = Hash
type Datum = Data
type DatumHash = Hash
type Redeemer = Data
type ScriptHash = Hash
type RedeemerHash = Hash
type CurrencySymbol = ByteString
type TokenName = ByteString
@deprecated("Use `PosixTime` instead", "0.7.0")
type POSIXTime = BigInt
type PosixTime = BigInt
@deprecated("Use `Interval` instead", "0.7.0")
type POSIXTimeRange = Interval
type PosixTimeRange = Interval

@deprecated("Not needed, use companion object of appropriate type instead")
object FromDataInstances {

    // given FromData[TxId] = (d: Data) => new TxId(unBData(unConstrData(d).snd.head))

    // given FromData[PubKeyHash] = (d: Data) => new PubKeyHash(unBData(d))

    // given FromData[TxOutRef] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new TxOutRef(fromData[TxId](args.head), unIData(args.tail.head))

    // given FromData[IntervalBoundType] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val tag = pair.fst
    //    if tag == BigInt(0) then IntervalBoundType.NegInf
    //    else if tag == BigInt(1) then new IntervalBoundType.Finite(unIData(pair.snd.head))
    //    else if tag == BigInt(2) then IntervalBoundType.PosInf
    //    else throw new Exception("Unknown IntervalBoundType tag")

    // given FromData[Credential] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val tag = pair.fst
    //    val args = pair.snd
    //    if tag == BigInt(0) then new Credential.PubKeyCredential(fromData[PubKeyHash](args.head))
    //    else if tag == BigInt(1) then new Credential.ScriptCredential(unBData(args.head))
    //    else throw new Exception("Unknown Credential tag")

    // given FromData[StakingCredential] =
    //    (d: Data) =>
    //        val pair = unConstrData(d)
    //        val tag = pair.fst
    //        val args = pair.snd
    //        if tag == BigInt(0) then
    //            new StakingCredential.StakingHash(fromData[Credential](args.head))
    //        else if tag == BigInt(1) then
    //            new StakingCredential.StakingPtr(
    //              unIData(args.head),
    //              unIData(args.tail.head),
    //              unIData(args.tail.tail.head)
    //            )
    //        else throw new RuntimeException("Invalid tag")

    // given FromData[DCert] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val tag = pair.fst
    //    val args = pair.snd
    //    if tag == BigInt(0) then new DCert.DelegRegKey(fromData[StakingCredential](args.head))
    //    else if tag == BigInt(1) then
    //        new DCert.DelegDeRegKey(fromData[StakingCredential](args.head))
    //    else if tag == BigInt(2) then
    //        new DCert.DelegDelegate(
    //          fromData[StakingCredential](args.head),
    //          fromData[PubKeyHash](args.tail.head)
    //        )
    //    else if tag == BigInt(3) then
    //        new DCert.PoolRegister(
    //          fromData[PubKeyHash](args.head),
    //          fromData[PubKeyHash](args.tail.head)
    //        )
    //    else if tag == BigInt(4) then
    //        new DCert.PoolRetire(
    //          fromData[PubKeyHash](args.head),
    //          unIData(args.tail.head)
    //        )
    //    else if tag == BigInt(5) then DCert.Genesis
    //    else if tag == BigInt(6) then DCert.Mir
    //    else throw new Exception("Unknown DCert tag")

    // given FromData[ScriptPurpose] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val tag = pair.fst
    //    val args = pair.snd
    //    if tag == BigInt(0) then new ScriptPurpose.Minting(fromData[TokenName](args.head))
    //    else if tag == BigInt(1) then new ScriptPurpose.Spending(fromData[TxOutRef](args.head))
    //    else if tag == BigInt(2) then
    //        new ScriptPurpose.Rewarding(fromData[StakingCredential](args.head))
    //    else if tag == BigInt(3) then new ScriptPurpose.Certifying(fromData[DCert](args.head))
    //    else throw new Exception("Unknown ScriptPurpose tag")

    // given FromData[Address] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new Address(
    //      fromData[Credential](args.head),
    //      fromData[Option[StakingCredential]](args.tail.head)
    //    )

    // given FromData[TxOut] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new TxOut(
    //      fromData[Address](args.head),
    //      fromData[Value](args.tail.head),
    //      fromData[Option[DatumHash]](args.tail.tail.head)
    //    )

    // given FromData[TxInInfo] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new TxInInfo(
    //      fromData[TxOutRef](args.head),
    //      fromData[TxOut](args.tail.head)
    //    )

    // given FromData[IntervalBound] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new IntervalBound(
    //      fromData[IntervalBoundType](args.head),
    //      fromData[Closure](args.tail.head)
    //    )

    // given FromData[Interval] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new Interval(
    //      fromData[IntervalBound](args.head),
    //      fromData[IntervalBound](args.tail.head)
    //    )

    // given FromData[TxInfo] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    val fromValue = summon[FromData[Value]]
    //    new TxInfo(
    //      fromData[List[TxInInfo]](args.head),
    //      fromData[List[TxOut]](args.tail.head),
    //      fromValue(args.tail.tail.head),
    //      fromValue(args.tail.tail.tail.head),
    //      fromData[List[DCert]](args.tail.tail.tail.tail.head),
    //      fromData[List[(StakingCredential, BigInt)]](args.tail.tail.tail.tail.tail.head),
    //      fromData[Interval](args.tail.tail.tail.tail.tail.tail.head),
    //      fromData[List[PubKeyHash]](args.tail.tail.tail.tail.tail.tail.tail.head),
    //      fromData[List[(DatumHash, Datum)]](args.tail.tail.tail.tail.tail.tail.tail.tail.head),
    //      fromData[TxId](args.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
    //    )

    // given FromData[ScriptContext] = (d: Data) =>
    //    val args = unConstrData(d).snd
    //    new ScriptContext(
    //      fromData[TxInfo](args.head),
    //      fromData[ScriptPurpose](args.tail.head)
    //    )

}

type Closure = Boolean

/** A type to represent the bounds of an interval.
  *   - `NegInf` represents negative infinity
  *   - `Finite(time)` represents a finite bound
  *   - `PosInf` represents positive infinity
  */
enum IntervalBoundType:
    case NegInf
    case Finite(time: PosixTime)
    case PosInf

@deprecated("Use `IntervalBoundType` instead", "0.7.0")
type Extended[T] = IntervalBoundType

@Compile
object IntervalBoundType {
    given Eq[IntervalBoundType] = (x: IntervalBoundType, y: IntervalBoundType) =>
        x match
            case NegInf =>
                y match
                    case NegInf => true
                    case _      => false
            case Finite(a) =>
                y match
                    case Finite(b) => a === b
                    case _         => false
            case PosInf =>
                y match
                    case PosInf => true
                    case _      => false

    given Ord[IntervalBoundType] = (x: IntervalBoundType, y: IntervalBoundType) =>
        x match
            case NegInf =>
                y match
                    case NegInf => Ord.Order.Equal
                    case _      => Ord.Order.Less
            case Finite(a) =>
                y match
                    case NegInf    => Ord.Order.Greater
                    case Finite(b) => a <=> b
                    case PosInf    => Ord.Order.Less
            case PosInf =>
                y match
                    case PosInf => Ord.Order.Equal
                    case _      => Ord.Order.Greater

    given ToData[IntervalBoundType] = ToData.derived

    given FromData[IntervalBoundType] = FromData.derived

}

/** An interval bound, either inclusive or exclusive.
  * @param boundType
  *   the type of the bound
  * @param isInclusive
  *   whether the bound is inclusive or not
  */
case class IntervalBound(boundType: IntervalBoundType, isInclusive: Closure)

@deprecated("Use `IntervalBound` instead", "0.7.0")
type UpperBound[A] = IntervalBound
@deprecated("Use `IntervalBound` instead", "0.7.0")
type LowerBound[A] = IntervalBound

@Compile
object IntervalBound:

    given Eq[IntervalBound] = (x: IntervalBound, y: IntervalBound) =>
        (x.boundType === y.boundType) && (x.isInclusive === y.isInclusive)

    given Ord[IntervalBound] = (x: IntervalBound, y: IntervalBound) =>
        (x.boundType <=> y.boundType) ifEqualThen (x.isInclusive <=> y.isInclusive)

    given ToData[IntervalBound] = ToData.derived

    given FromData[IntervalBound] = FromData.derived

end IntervalBound

/** A type to represent time intervals.
  * @param from
  *   the lower bound of the interval
  * @param to
  *   the upper bound of the interval
  */
case class Interval(from: IntervalBound, to: IntervalBound)

@Compile
object Interval:

    given Eq[Interval] = (x: Interval, y: Interval) => (x.from === y.from) && (x.to === y.to)

    given Ord[Interval] = (x: Interval, y: Interval) =>
        (x.from <=> y.from) ifEqualThen (x.to <=> y.to)

    given ToData[Interval] = ToData.derived

    given FromData[Interval] = FromData.derived

    /** Inclusive -∞ interval bound */
    val negInf: IntervalBound = new IntervalBound(IntervalBoundType.NegInf, true)

    /** Inclusive +∞ interval bound */
    val posInf: IntervalBound = new IntervalBound(IntervalBoundType.PosInf, true)

    /** Inclusive -∞ to +∞ interval */
    val always: Interval = new Interval(negInf, posInf)

    @deprecated("Use `finite` instead", "0.7.0")
    def lowerBound[A](a: PosixTime): LowerBound[A] =
        new IntervalBound(new IntervalBoundType.Finite(a), true)
    @deprecated("Use `finite` instead", "0.7.0")
    def upperBound[A](a: PosixTime): UpperBound[A] =
        new IntervalBound(new IntervalBoundType.Finite(a), true)

    /** Create a finite inclusive interval bound */
    def finite(time: PosixTime): IntervalBound =
        new IntervalBound(new IntervalBoundType.Finite(time), true)

    @deprecated("Use `after` instead", "0.7.0")
    def from[A](a: PosixTime): Interval =
        new Interval(finite(a), posInf)

    @deprecated("Use `before` instead", "0.7.0")
    def to[A](a: PosixTime): Interval =
        new Interval(negInf, finite(a))

    /** Creates an interval that includes all values greater than the given bound. i.e
      * [lower_bound,+∞)
      */
    def after(time: PosixTime): Interval =
        new Interval(finite(time), posInf)

    /** Creates an interval that includes all values less than the given bound. i.e (-∞,upper_bound]
      */
    def before(time: PosixTime): Interval =
        new Interval(negInf, finite(time))

    /** Creates an interval that includes all values between the given bounds. i.e [lower_bound,
      * upper_bound]
      */
    def between(from: PosixTime, to: PosixTime): Interval =
        new Interval(finite(from), finite(to))

    /** An empty interval that contains no values, ∄ posixTime ∣ never.contains(posixTime)
      */
    val never: Interval = {
        /*
         * Not the biggest fan of using the sentinel neginf here, other options are using an enum to represent the
         * `never`. Invariants expressed in IntervalTest should hold regardless of the implementation
         */
        val negInfExclusive = new IntervalBound(NegInf, false)
        new Interval(negInfExclusive, negInfExclusive)
    }

    extension (self: Interval)

        def contains(time: PosixTime): Boolean = {
            val aboveFrom = {
                self.from.boundType match {
                    case IntervalBoundType.Finite(from) =>
                        if self.from.isInclusive then time >= from else time > from
                    case IntervalBoundType.NegInf => true
                    case IntervalBoundType.PosInf => false
                }
            }
            val belowTo = {
                self.to.boundType match {
                    case IntervalBoundType.Finite(to) =>
                        if self.to.isInclusive then time <= to else time < to
                    case IntervalBoundType.NegInf => false
                    case IntervalBoundType.PosInf => true
                }
            }

            aboveFrom && belowTo
        }

        /** Checks if an interval is entirely after a given time. Returns true if all values in the
          * interval are greater than the given time.
          */
        def entirelyAfter(time: PosixTime): Boolean =
            self match
                case Interval(from, to) =>
                    val negInfExclusive = new IntervalBound(NegInf, false)
                    if from === negInfExclusive && to === negInfExclusive then {
                        false
                    } else
                        from.boundType match
                            case IntervalBoundType.NegInf => false
                            case IntervalBoundType.PosInf => true
                            case IntervalBoundType.Finite(fromTime) =>
                                if from.isInclusive then fromTime > time
                                else fromTime >= time

        /** Checks if an interval is entirely before a given time. Returns true if all values in the
          * interval are less than the given time.
          */
        def entirelyBefore(time: PosixTime): Boolean =
            self match
                case Interval(from, to) =>
                    val negInfExclusive = new IntervalBound(NegInf, false)
                    if from === negInfExclusive && to === negInfExclusive then {
                        false
                    } else
                        to.boundType match
                            case IntervalBoundType.NegInf => true
                            case IntervalBoundType.PosInf => false
                            case IntervalBoundType.Finite(toTime) =>
                                if to.isInclusive then toTime < time
                                else toTime <= time

        /** Checks if an interval is entirely between two given times. Returns true if all values in
          * the interval are between the given times.
          */
        def entirelyBetween(after: PosixTime, before: PosixTime): Boolean =
            entirelyAfter(after) && entirelyBefore(before)

    end extension

end Interval

enum DCert:
    case DelegRegKey(cred: StakingCredential)
    case DelegDeRegKey(cred: StakingCredential)
    case DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash)
    case PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash)
    case PoolRetire(poolId: PubKeyHash, epoch: BigInt)
    case Genesis
    case Mir

@Compile
object DCert {

    given Eq[DCert] = (x: DCert, y: DCert) =>
        x match
            case DCert.DelegRegKey(cred) =>
                y match
                    case DCert.DelegRegKey(cred) => cred === cred
                    case _                       => false

            case DCert.DelegDeRegKey(cred) =>
                y match
                    case DCert.DelegDeRegKey(cred) => cred === cred
                    case _                         => false
            case DCert.DelegDelegate(cred, delegatee) =>
                y match
                    case DCert.DelegDelegate(cred, delegatee) =>
                        cred === cred && delegatee === delegatee
                    case _ => false
            case DCert.PoolRegister(poolId, vrf) =>
                y match
                    case DCert.PoolRegister(poolId, vrf) => poolId === poolId && vrf === vrf
                    case _                               => false
            case DCert.PoolRetire(poolId, epoch) =>
                y match
                    case DCert.PoolRetire(poolId, epoch) => poolId === poolId && epoch === epoch
                    case _                               => false
            case DCert.Genesis =>
                y match
                    case DCert.Genesis => true
                    case _             => false
            case DCert.Mir =>
                y match
                    case DCert.Mir => true
                    case _         => false

    given Ord[DCert] = (x: DCert, y: DCert) =>
        x match
            case DCert.DelegRegKey(cred1) =>
                y match
                    case DCert.DelegRegKey(cred2) => cred1 <=> cred2
                    case _                        => Ord.Order.Less

            case DCert.DelegDeRegKey(cred1) =>
                y match
                    case DCert.DelegRegKey(_)       => Ord.Order.Greater
                    case DCert.DelegDeRegKey(cred2) => cred1 <=> cred2
                    case _                          => Ord.Order.Less

            case DCert.DelegDelegate(cred1, del1) =>
                y match
                    case DCert.DelegRegKey(_)   => Ord.Order.Greater
                    case DCert.DelegDeRegKey(_) => Ord.Order.Greater
                    case DCert.DelegDelegate(cred2, del2) =>
                        (cred1 <=> cred2) ifEqualThen (del1 <=> del2)
                    case _ => Ord.Order.Less

            case DCert.PoolRegister(id1, vrf1) =>
                y match
                    case DCert.DelegRegKey(_)      => Ord.Order.Greater
                    case DCert.DelegDeRegKey(_)    => Ord.Order.Greater
                    case DCert.DelegDelegate(_, _) => Ord.Order.Greater
                    case DCert.PoolRegister(id2, vrf2) =>
                        (id1 <=> id2) ifEqualThen (vrf1 <=> vrf2)
                    case _ => Ord.Order.Less

            case DCert.PoolRetire(id1, epoch1) =>
                y match
                    case DCert.DelegRegKey(_)      => Ord.Order.Greater
                    case DCert.DelegDeRegKey(_)    => Ord.Order.Greater
                    case DCert.DelegDelegate(_, _) => Ord.Order.Greater
                    case DCert.PoolRegister(_, _)  => Ord.Order.Greater
                    case DCert.PoolRetire(id2, epoch2) =>
                        (id1 <=> id2) ifEqualThen (epoch1 <=> epoch2)
                    case _ => Ord.Order.Less

            case DCert.Genesis =>
                y match
                    case DCert.DelegRegKey(_)      => Ord.Order.Greater
                    case DCert.DelegDeRegKey(_)    => Ord.Order.Greater
                    case DCert.DelegDelegate(_, _) => Ord.Order.Greater
                    case DCert.PoolRegister(_, _)  => Ord.Order.Greater
                    case DCert.PoolRetire(_, _)    => Ord.Order.Greater
                    case DCert.Genesis             => Ord.Order.Equal
                    case DCert.Mir                 => Ord.Order.Less

            case DCert.Mir =>
                y match
                    case DCert.Mir => Ord.Order.Equal
                    case _         => Ord.Order.Greater

    given ToData[DCert] = ToData.derived

    given FromData[DCert] = FromData.derived

}

case class TxId(hash: Hash):
    override def toString = s"TxId(${hash.toHex})"

@Compile
object TxId:

    given Eq[TxId] = (a: TxId, b: TxId) => a.hash === b.hash
    given Ord[TxId] = (a: TxId, b: TxId) => a.hash <=> b.hash

    given ToData[TxId] = ToData.derived
    given FromData[TxId] = FromData.derived

end TxId

case class TxOutRef(id: TxId, idx: BigInt)

@Compile
object TxOutRef {

    given Eq[TxOutRef] = (a: TxOutRef, b: TxOutRef) => a.id === b.id && a.idx === b.idx

    given Ord[TxOutRef] = (a: TxOutRef, b: TxOutRef) =>
        (a.id <=> b.id) ifEqualThen (a.idx <=> b.idx)

    given ToData[TxOutRef] = ToData.derived

    given FromData[TxOutRef] = FromData.derived

}

case class PubKeyHash(hash: Hash) {
    override def toString = s"pkh#${hash}"
}

@Compile
object PubKeyHash {

    given Eq[PubKeyHash] = (a: PubKeyHash, b: PubKeyHash) => a.hash === b.hash

    given Ord[PubKeyHash] = (a: PubKeyHash, b: PubKeyHash) => a.hash <=> b.hash

    given ToData[PubKeyHash] = (a: PubKeyHash) => summon[ToData[ByteString]](a.hash)

    given FromData[PubKeyHash] = (d: Data) => new PubKeyHash(unBData(d))

}

enum Credential:
    case PubKeyCredential(hash: PubKeyHash)
    case ScriptCredential(hash: ValidatorHash)

@Compile
object Credential {

    given Eq[Credential] = (a: Credential, b: Credential) =>
        a match
            case Credential.PubKeyCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(hash2) => hash.hash === hash2.hash
                    case Credential.ScriptCredential(hash)  => false
            case Credential.ScriptCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(hash2) => false
                    case Credential.ScriptCredential(hash2) => hash === hash2

    given Ord[Credential] = (a: Credential, b: Credential) =>
        a match
            case Credential.PubKeyCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(hash2) => hash <=> hash2
                    case Credential.ScriptCredential(_)     => Ord.Order.Less
            case Credential.ScriptCredential(hash) =>
                b match
                    case Credential.PubKeyCredential(_)     => Ord.Order.Greater
                    case Credential.ScriptCredential(hash2) => hash <=> hash2

    given FromData[Credential] = FromData.derived

    given ToData[Credential] = ToData.derived
}

enum StakingCredential:
    case StakingHash(cred: Credential)
    case StakingPtr(a: BigInt, b: BigInt, c: BigInt)

@Compile
object StakingCredential {
    given Eq[StakingCredential] = (lhs: StakingCredential, rhs: StakingCredential) =>
        lhs match
            case StakingCredential.StakingHash(cred) =>
                rhs match
                    case StakingCredential.StakingHash(cred2)  => cred === cred2
                    case StakingCredential.StakingPtr(a, b, c) => false
            case StakingCredential.StakingPtr(a, b, c) =>
                rhs match
                    case StakingCredential.StakingHash(cred2) => false
                    case StakingCredential.StakingPtr(a2, b2, c2) =>
                        a === a2 && b === b2 && c === c2

    given Ord[StakingCredential] = (lhs: StakingCredential, rhs: StakingCredential) =>
        lhs match
            case StakingCredential.StakingHash(cred) =>
                rhs match
                    case StakingCredential.StakingHash(cred2)  => cred <=> cred2
                    case StakingCredential.StakingPtr(_, _, _) => Ord.Order.Less
            case StakingCredential.StakingPtr(a, b, c) =>
                rhs match
                    case StakingCredential.StakingHash(_) => Ord.Order.Greater
                    case StakingCredential.StakingPtr(a2, b2, c2) =>
                        (a <=> a2) ifEqualThen (b <=> b2) ifEqualThen (c <=> c2)

    given FromData[StakingCredential] = FromData.derived

    given ToData[StakingCredential] = ToData.derived
}

case class Address(
    credential: Credential,
    stakingCredential: Option[StakingCredential]
)

@Compile
object Address {

    given Eq[Address] = (a: Address, b: Address) =>
        a.credential === b.credential && a.stakingCredential === b.stakingCredential

    given Ord[Address] = (a: Address, b: Address) =>
        (a.credential <=> b.credential) ifEqualThen (a.stakingCredential <=> b.stakingCredential)

    given ToData[Address] = ToData.derived

    given FromData[Address] = FromData.derived

}

case class TxOut(address: Address, value: Value, datumHash: Option[DatumHash]) {

    override def toString: String = {
        s"""TxOut(
           |  address: $address,
           |  value: ${Value.debugToString(value)},
           |  datumHash: $datumHash
           |)""".stripMargin
    }

}

@Compile
object TxOut {
    given Eq[TxOut] = (a: TxOut, b: TxOut) =>
        a.address === b.address && a.value === b.value && a.datumHash === b.datumHash

    given Ord[TxOut] = (a: TxOut, b: TxOut) =>
        given Ord[Value] = Value.valueOrd
        (a.address <=> b.address) ifEqualThen (a.value <=> b.value) ifEqualThen (a.datumHash <=> b.datumHash)

    given ToData[TxOut] = ToData.derived

    given FromData[TxOut] = FromData.derived

}

case class TxInInfo(
    outRef: TxOutRef,
    resolved: TxOut
)

@Compile
object TxInInfo {
    given Eq[TxInInfo] = (a: TxInInfo, b: TxInInfo) =>
        a.outRef === b.outRef && a.resolved === b.resolved

    given Ord[TxInInfo] = (a: TxInInfo, b: TxInInfo) =>
        (a.outRef <=> b.outRef) ifEqualThen (a.resolved <=> b.resolved)

    given ToData[TxInInfo] = ToData.derived

    given FromData[TxInInfo] = FromData.derived

}

case class TxInfo(
    inputs: List[TxInInfo],
    outputs: List[TxOut],
    fee: Value,
    mint: Value,
    dcert: List[DCert],
    withdrawals: List[(StakingCredential, BigInt)],
    validRange: PosixTimeRange,
    signatories: List[PubKeyHash],
    data: List[(DatumHash, Datum)],
    id: TxId
) {

    override def toString: String = {
        s"""TxInfo(
           |  inputs: ${inputs.asScala.mkString("[", ", ", "]")},
           |  outputs: ${outputs.asScala.mkString("[", ", ", "]")},
           |  fee: ${Value.debugToString(fee)},
           |  mint: ${Value.debugToString(mint)},
           |  dcert: $dcert,
           |  withdrawals: $withdrawals,
           |  validRange: $validRange,
           |  signatories: $signatories,
           |  data: $data,
           |  id: $id
           |)""".stripMargin
    }

}

@Compile
object TxInfo {
    val placeholder: TxInfo = TxInfo(
      inputs = List.empty,
      outputs = List.empty,
      fee = Value.zero,
      mint = Value.zero,
      dcert = List.empty,
      withdrawals = List.empty,
      validRange = Interval.always,
      signatories = List.empty,
      data = List.empty,
      id = TxId(hex"0000000000000000000000000000000000000000000000000000000000000000")
    )

    given Eq[TxInfo] = (x: TxInfo, y: TxInfo) =>
        x.inputs === y.inputs &&
            x.outputs === y.outputs &&
            x.fee === y.fee &&
            x.mint === y.mint &&
            x.dcert === y.dcert &&
            x.withdrawals === y.withdrawals &&
            x.validRange === y.validRange &&
            x.signatories === y.signatories &&
            x.data === y.data &&
            x.id === y.id

    given Ord[TxInfo] = (x: TxInfo, y: TxInfo) =>
        given Ord[Value] = Value.valueOrd
        (x.inputs <=> y.inputs) ifEqualThen
            (x.outputs <=> y.outputs) ifEqualThen
            (x.fee <=> y.fee) ifEqualThen
            (x.mint <=> y.mint) ifEqualThen
            (x.dcert <=> y.dcert) ifEqualThen
            (x.withdrawals <=> y.withdrawals) ifEqualThen
            (x.validRange <=> y.validRange) ifEqualThen
            (x.signatories <=> y.signatories) ifEqualThen
            (x.data <=> y.data) ifEqualThen
            (x.id <=> y.id)

    given ToData[TxInfo] = ToData.derived
    given FromData[TxInfo] = FromData.derived

    extension (self: TxInfo) {

        /** Finds an input by its out reference.
          *
          * @param outRef
          *   The output reference to find.
          * @return
          *   An `Option` containing the found input, or `None` if not found.
          */
        def findOwnInput(outRef: TxOutRef): Option[TxInInfo] = {
            Utils.findInput(self.inputs, outRef)
        }

        /** Finds all outputs that match a given script hash.
          *
          * @param scriptHash
          *   The script hash to match against the outputs' addresses.
          * @return
          *   A list of outputs that match the script hash.
          */
        def findOwnScriptOutputs(scriptHash: ValidatorHash): List[TxOut] = {
            Utils.findScriptOutputs(self.outputs, scriptHash)
        }
    }
}

enum ScriptPurpose:
    case Minting(curSymbol: ByteString)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(stakingCred: StakingCredential)
    case Certifying(cert: DCert)

@Compile
object ScriptPurpose {

    given Eq[ScriptPurpose] = (x: ScriptPurpose, y: ScriptPurpose) =>
        x match
            case ScriptPurpose.Minting(curSymbol) =>
                y match
                    case ScriptPurpose.Minting(curSymbol) => curSymbol === curSymbol
                    case _                                => false
            case ScriptPurpose.Spending(txOutRef) =>
                y match
                    case ScriptPurpose.Spending(txOutRef) => txOutRef === txOutRef
                    case _                                => false
            case ScriptPurpose.Rewarding(stakingCred) =>
                y match
                    case ScriptPurpose.Rewarding(stakingCred) => stakingCred === stakingCred
                    case _                                    => false
            case ScriptPurpose.Certifying(cert) =>
                y match
                    case ScriptPurpose.Certifying(cert) => cert === cert
                    case _                              => false

    given Ord[ScriptPurpose] = (x: ScriptPurpose, y: ScriptPurpose) =>
        x match
            case ScriptPurpose.Minting(sym1) =>
                y match
                    case ScriptPurpose.Minting(sym2) => sym1 <=> sym2
                    case _                           => Ord.Order.Less

            case ScriptPurpose.Spending(ref1) =>
                y match
                    case ScriptPurpose.Minting(_)     => Ord.Order.Greater
                    case ScriptPurpose.Spending(ref2) => ref1 <=> ref2
                    case _                            => Ord.Order.Less

            case ScriptPurpose.Rewarding(cred1) =>
                y match
                    case ScriptPurpose.Minting(_)       => Ord.Order.Greater
                    case ScriptPurpose.Spending(_)      => Ord.Order.Greater
                    case ScriptPurpose.Rewarding(cred2) => cred1 <=> cred2
                    case _                              => Ord.Order.Less

            case ScriptPurpose.Certifying(cert1) =>
                y match
                    case ScriptPurpose.Certifying(cert2) => cert1 <=> cert2
                    case _                               => Ord.Order.Greater

    given ToData[ScriptPurpose] = ToData.derived

    given FromData[ScriptPurpose] = FromData.derived

}

case class ScriptContext(txInfo: TxInfo, purpose: ScriptPurpose)

@Compile
object ScriptContext:
    given Eq[ScriptContext] = (x: ScriptContext, y: ScriptContext) =>
        x.txInfo === y.txInfo && x.purpose === y.purpose

    given Ord[ScriptContext] = (x: ScriptContext, y: ScriptContext) =>
        (x.txInfo <=> y.txInfo) ifEqualThen (x.purpose <=> y.purpose)

    given FromData[ScriptContext] = FromData.derived

    given ToData[ScriptContext] = ToData.derived

end ScriptContext

@Compile
object Utils {

    /** Finds an input in the list of inputs by its out reference.
      *
      * @param inputs
      *   The list of inputs to search in.
      * @param outRef
      *   The output reference to find.
      * @return
      *   An `Option` containing the found input, or `None` if not found.
      */
    def findInput(inputs: List[TxInInfo], outRef: TxOutRef): Option[TxInInfo] = {
        inputs.find(_.outRef === outRef)
    }

    /** Finds all outputs that match a given script hash.
      *
      * @param outputs
      *   The list of outputs to search in.
      * @param scriptHash
      *   The script hash to match against the outputs' addresses.
      * @return
      *   A list of outputs that match the script hash.
      */
    def findScriptOutputs(outputs: List[TxOut], scriptHash: ValidatorHash): List[TxOut] = {
        outputs.filter { output =>
            output.address.credential match
                case Credential.ScriptCredential(hash) => hash === scriptHash
                case _                                 => false
        }
    }
}
