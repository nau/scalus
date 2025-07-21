package scalus.prelude

import scalus.Compile
import scalus.Ignore
import scalus.builtin.Builtins.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.macros.Macros

import scala.annotation.nowarn
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}

extension [A](self: A)
    inline def let[B](inline fn: A => B): B = fn(self)
    inline def also[B](inline callback: A => Unit): A = { callback(self); self }

extension (x: Boolean)
    /** Trace the expression only if it evaluates to `false`. This is useful to trace an entire
      * evaluation path that led to a final expression being `false`.
      * @example
      *   {{{mustBeAfter.? && mustSpendToken.?}}}
      *
      * will trace "mustSpendToken ? False" if `mustBeAfter` is `true` and `mustSpendToken` is
      * `false`.
      *
      * @return
      *   the value of the expression
      */
    inline def ? : Boolean = ${ Macros.questionMark('x) }

    inline infix def orFail(inline message: String): Unit =
        if x then () else fail(message)

extension (self: BigInt)
    def to(other: BigInt): List[BigInt] = List.range(self, other)
    def until(other: BigInt): List[BigInt] = List.rangeUntil(self, other)

type Eq[-A] = (A, A) => Boolean

// given Eq[Nothing] = (x: Nothing, y: Nothing) => throw new Exception("EQN")
inline given Eq[BigInt] = equalsInteger
inline given Eq[ByteString] = equalsByteString
inline given Eq[String] = equalsString
@nowarn
inline given Eq[Boolean] = _ == _
inline given Eq[Data] = equalsData
@nowarn
inline given Eq[Unit] = (_: Unit, _: Unit) => true

val Eq: EqCompanion.type = EqCompanion

@Compile
object EqCompanion:
    inline def apply[A: Eq]: Eq[A] = summon[Eq[A]]

    def by[A, B: Eq](mapper: A => B): Eq[A] = (lhs: A, rhs: A) => mapper(lhs) === mapper(rhs)

    extension [A](self: Eq[A])
        inline def eqv(inline lhs: A, inline rhs: A): Boolean = self(lhs, rhs)
        inline def notEqv(inline lhs: A, inline rhs: A): Boolean = !self.eqv(lhs, rhs)

        def orElse(other: Eq[A]): Eq[A] = (lhs: A, rhs: A) =>
            if self.eqv(lhs, rhs) then other.eqv(lhs, rhs) else false

        def orElseBy[B: Eq](mapper: A => B): Eq[A] = (lhs: A, rhs: A) =>
            if self.eqv(lhs, rhs) then by[A, B](mapper).eqv(lhs, rhs) else false

    end extension

    given [A: Eq, B: Eq]: Eq[(A, B)] = Eq.by[(A, B), A](_._1).orElseBy(_._2)

    def keyPairEq[A: Eq, B]: Eq[(A, B)] = Eq.by[(A, B), A](_._1)

end EqCompanion

extension [A](x: A)
    inline def ===(inline y: A)(using inline eq: Eq[A]): Boolean = eq(x, y)
    inline def !==(inline y: A)(using inline eq: Eq[A]): Boolean = !eq(x, y)

type Ord[-A] = (A, A) => Ord.Order

val Ord: OrdCompanion.type = OrdCompanion

@Compile
object OrdCompanion:
    inline def apply[A: Ord]: Ord[A] = summon[Ord[A]]

    enum Order:
        case Less, Greater, Equal

    import Order.*

    extension (self: Order)
        def isLess: Boolean = self match { case Less => true; case _ => false }
        def isLessEqual: Boolean = self match {
            case Less => true; case Equal => true; case _ => false
        }
        def isGreater: Boolean = self match { case Greater => true; case _ => false }
        def isGreaterEqual: Boolean = self match {
            case Greater => true; case Equal => true; case _ => false
        }
        def isEqual: Boolean = self match { case Equal => true; case _ => false }
        inline def nonEqual: Boolean = !isEqual

    end extension

    given Eq[Order] = (lhs, rhs) =>
        lhs match
            case Less    => rhs.isLess
            case Greater => rhs.isGreater
            case Equal   => rhs.isEqual

    extension [A: Ord](self: A)
        inline def <=>(inline other: A): Order = Ord[A].compare(self, other)
        def lt(other: A): Boolean = (self <=> other).isLess
        def lteq(other: A): Boolean = (self <=> other).isLessEqual
        def gt(other: A): Boolean = (self <=> other).isGreater
        def gteq(other: A): Boolean = (self <=> other).isGreaterEqual
        def equiv(other: A): Boolean = (self <=> other).isEqual

    end extension

    def by[A, B: Ord](mapper: A => B): Ord[A] = (lhs: A, rhs: A) => mapper(lhs) <=> mapper(rhs)

    extension [A](self: Ord[A])
        inline def compare(inline lhs: A, inline rhs: A): Order = self(lhs, rhs)

        def orElse(other: Ord[A]): Ord[A] = (lhs: A, rhs: A) =>
            val order = self.compare(lhs, rhs)
            if order.nonEqual then order else other.compare(lhs, rhs)

        def orElseBy[B: Ord](mapper: A => B): Ord[A] = (lhs: A, rhs: A) =>
            val order = self.compare(lhs, rhs)
            if order.nonEqual then order else by[A, B](mapper).compare(lhs, rhs)

    end extension

    given Ord[ByteString] = (x: ByteString, y: ByteString) =>
        if lessThanByteString(x, y) then Less
        else if equalsByteString(x, y) then Equal
        else Greater

    given Ord[BigInt] = (x: BigInt, y: BigInt) =>
        if lessThanInteger(x, y) then Less else if lessThanInteger(y, x) then Greater else Equal

    given [A: Ord, B: Ord]: Ord[(A, B)] = Ord.by[(A, B), A](_._1).orElseBy(_._2)

    def keyPairOrd[A: Ord, B]: Ord[(A, B)] = Ord.by[(A, B), A](_._1)

end OrdCompanion

inline def log(msg: String): Unit = trace(msg)(())
inline def identity[A](value: A): A = value

@Compile
object Prelude {
    @deprecated("Use `scalus.prelude.Eq` instead")
    type Eq[-A] = (A, A) => Boolean
    @deprecated("Use `scalus.prelude.Eq[BigInt]` instead")
    // given Eq[Nothing] = (x: Nothing, y: Nothing) => throw new Exception("EQN")
    given Eq[BigInt] = (x: BigInt, y: BigInt) => equalsInteger(x, y)
    @deprecated("Use `scalus.prelude.Eq[ByteString]` instead")
    given Eq[ByteString] = (x: ByteString, y: ByteString) => equalsByteString(x, y)
    @deprecated("Use `scalus.prelude.Eq[String]` instead")
    given Eq[String] = (x: String, y: String) => equalsString(x, y)
    @deprecated("Use `scalus.prelude.Eq[Boolean]` instead")
    given Eq[Boolean] = (x: Boolean, y: Boolean) => x == y
    @deprecated("Use `scalus.prelude.Eq[Data]` instead")
    given Eq[Data] = (x: Data, y: Data) => equalsData(x, y)
    @deprecated("Use `scalus.prelude.Eq[Unit]` instead")
    given Eq[Unit] = (_: Unit, _: Unit) => true

    extension [A](x: A)
        @deprecated("Use `scalus.prelude.===` instead") inline def ===(inline y: A)(using
            inline eq: Eq[A]
        ): Boolean = eq(x, y)
    extension [A](x: A)
        @deprecated("Use `scalus.prelude.!==` instead") inline def !==(inline y: A)(using
            inline eq: Eq[A]
        ): Boolean = !eq(x, y)

    def encodeHex(input: ByteString): String = {
        import ByteString.*
        val len = lengthOfByteString(input)

        val byteToChar =
            (byte: BigInt) => if byte < 10 then byte + 48 else byte + 87

        def go(i: BigInt): ByteString = {
            if i == len then ByteString.fromHex("")
            else {
                val byte = indexByteString(input, i)
                val char1 = byteToChar(byte / 16)
                val char2 = byteToChar(byte % 16)
                char1 +: char2 +: go(i + 1)
            }
        }
        decodeUtf8(go(0))
    }

    @deprecated("Use `scalus.prelude.log` instead")
    inline def log(msg: String): Unit = trace(msg)(())

    extension (b: Boolean)
        @deprecated("Use `scalus.prelude.orFail` instead")
        inline infix def orFail(inline message: String): Unit =
            if b then () else fail(message)
}

/** Tests an expression, throwing an `RequirementError` if false.
  * @param requirement
  *   the expression to test
  * @throws RequirementError
  *   when invoked off-chain.
  * @note
  *   we do not use scala.Predef.require because it's not an `inline` method and it's not expanded
  *   before Scalus compiler plugin phase.
  * @example
  *   {{{
  *   require(value > 1000, "Not enough")
  *   }}}
  */
inline def require(inline requirement: Boolean, inline message: String): Unit =
    if requirement then () else throw new RequirementError(message)

/** Tests an expression, throwing a `RequirementError` if false.
  *
  * This is used to enforce preconditions in on-chain logic.
  *
  * @param requirement
  *   The boolean expression to test.
  * @throws RequirementError
  *   when invoked off-chain.
  * @example
  *   {{{
  *   require(value > 1000)
  *   }}}
  */
inline def require(inline requirement: Boolean): Unit =
    if requirement then () else throw new RequirementError()

/** Fails the onchain evaluation with an `ERROR` term and a specific error message.
  *
  * This is used to indicate a failure in the on-chain logic with a specific error message.
  *
  * @param message
  *   The error message to include in the failure.
  * @throws OnchainError
  *   when invoked off-chain.
  */
inline def fail(inline message: String): Nothing = throw new OnchainError(message)

/** Fails the onchain evaluation with an `ERROR` term.
  *
  * This is used to indicate a failure in the on-chain logic without providing a specific error
  * message.
  *
  * @throws OnchainError
  *   when invoked off-chain.
  */
inline def fail(): Nothing = throw new OnchainError()

/** Fails the onchain evaluation with an `ERROR` term indicating an impossible situation.
  *
  * This is used to indicate an impossible situation in the on-chain logic.
  *
  * @throws ImpossibleLedgerStateError
  *   when invoked off-chain.
  */
inline def impossible(): Nothing = throw new ImpossibleLedgerStateError

/** `???` can be used for marking methods that remain to be implemented.
  * @throws NotImplementedError
  *   when `???` is invoked.
  */
inline def ??? : Nothing = throw new NotImplementedError

@deprecated("Use `scalus.prelude.Option` instead")
enum Maybe[+A]:
    @deprecated("Use `scalus.prelude.Option.None` instead") case Nothing extends Maybe[Nothing]
    @deprecated("Use `scalus.prelude.Option.Some` instead") case Just(value: A)

@Compile
@deprecated("Use `scalus.prelude.Option` instead")
object Maybe {

    /** Constructs a `Maybe` from a value. If the value is `null`, it returns `Nothing`, otherwise
      * `Just(value)`.
      */
    @Ignore
    @deprecated("Use `scalus.prelude.Option.apply` instead")
    inline def apply[A](x: A): Maybe[A] = if x == null then Nothing else Just(x)

    extension [A](m: Maybe[A])
        /** Converts a `Maybe` to an [[Option]] */
        @Ignore
        @deprecated("Use `scalus.prelude.Option.asScala` instead")
        def toOption: scala.Option[A] = m match
            case Nothing => scala.None
            case Just(a) => scala.Some(a)

        @deprecated("Use `scalus.prelude.Option.map` instead")
        def map[B](f: A => B): Maybe[B] = m match
            case Nothing => Nothing
            case Just(a) => Just(f(a))

    /** Converts an [[Option]] to a `Maybe` */
    @Ignore
    @deprecated("Use `scalus.prelude.Option.asScalus` instead")
    def fromOption[A](o: scala.Option[A]): Maybe[A] = o match
        case scala.None    => Nothing
        case scala.Some(a) => Just(a)

    @deprecated("Use `scalus.prelude.Option.optionEq` instead")
    given maybeEq[A](using eq: Eq[A]): Eq[Maybe[A]] = (a: Maybe[A], b: Maybe[A]) =>
        a match
            case Nothing =>
                b match
                    case Nothing => true
                    case Just(a) => false
            case Just(value) =>
                b match
                    case Nothing      => false
                    case Just(value2) => value === value2
}

enum These[+A, +B]:
    case This(a: A)
    case That(b: B)
    case These(a: A, b: B)

@Compile
object These {
    given [A: Eq, B: Eq]: Eq[scalus.prelude.These[A, B]] =
        (lhs: scalus.prelude.These[A, B], rhs: scalus.prelude.These[A, B]) =>
            lhs match
                case scalus.prelude.These.This(a) =>
                    rhs match
                        case scalus.prelude.These.This(b) => a === b
                        case _                            => false
                case scalus.prelude.These.That(b) =>
                    rhs match
                        case scalus.prelude.These.That(c) => b === c
                        case _                            => false
                case scalus.prelude.These.These(a, b) =>
                    rhs match
                        case scalus.prelude.These.These(c, d) => a === c && b === d
                        case _                                => false
}

case class Rational(numerator: BigInt, denominator: BigInt)

@Compile
object Rational:

    given Eq[Rational] = (lhs: Rational, rhs: Rational) =>
        lhs.numerator * rhs.denominator === rhs.numerator * lhs.denominator

    given rationalFromData: FromData[Rational] = FromData.derived

    given rationalToData: ToData[Rational] = ToData.derived
