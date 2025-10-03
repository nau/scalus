package scalus.prelude

import scalus.{Compile, Ignore}
import scalus.builtin.Builtins.*
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.macros.Macros

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

    @deprecated("Use prelude.require() instead", "0.13.0")
    inline infix def orFail(inline message: String): Unit =
        if x then () else fail(message)

extension (self: BigInt)
    def to(other: BigInt): List[BigInt] = List.range(self, other)
    def until(other: BigInt): List[BigInt] = List.rangeUntil(self, other)

inline def log(inline msg: String): Unit = trace(msg)(())
inline def identity[A](inline value: A): A = value

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
            if i == len then hex""
            else
                val byte = indexByteString(input, i)
                val char1 = byteToChar(byte / 16)
                val char2 = byteToChar(byte % 16)
                char1 +: char2 +: go(i + 1)
        }
        decodeUtf8(go(0))
    }

    def showBigInt(input: BigInt): String = {
        // Convert BigInt to String representation using only built-in methods and prelude types
        val isNegative = input < 0
        val absValue = if isNegative then -input else input

        def digitToString(digit: BigInt): String = {
            if digit == BigInt(0) then "0"
            else if digit == BigInt(1) then "1"
            else if digit == BigInt(2) then "2"
            else if digit == BigInt(3) then "3"
            else if digit == BigInt(4) then "4"
            else if digit == BigInt(5) then "5"
            else if digit == BigInt(6) then "6"
            else if digit == BigInt(7) then "7"
            else if digit == BigInt(8) then "8"
            else if digit == BigInt(9) then "9"
            else fail("Not a valid digit")
        }

        def go(value: BigInt): String = {
            val nextValue = value / 10
            val digit = digitToString(value % 10)
            if nextValue == BigInt(0) then digit
            else appendString(go(nextValue), digit)
        }
        val result = go(absValue)
        if isNegative then appendString("-", result) else result
    }

    @deprecated("Use `scalus.prelude.log` instead")
    inline def log(inline msg: String): Unit = trace(msg)(())

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

    given Ord[Rational] = (lhs: Rational, rhs: Rational) =>
        lhs.numerator * rhs.denominator <=> rhs.numerator * lhs.denominator

    given rationalFromData: FromData[Rational] = FromData.derived

    given rationalToData: ToData[Rational] = ToData.derived

extension [A](self: scala.Seq[A]) {

    /** Converts a [[scala.Seq]] to a `List`.
      *
      * This method is only available offchain.
      *
      * @return
      *   A `List[A]` containing all the elements from this sequence in the same order.
      * @example
      *   {{{
      *   scala.Seq.empty[BigInt].asScalus === List.empty[BigInt]
      *
      *   val seq: scala.Seq[BigInt] = scala.Seq(BigInt(1), BigInt(2), BigInt(3))
      *   seq.asScalus === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   }}}
      */
    @Ignore
    def asScalus: List[A] = self match
        case scala.Seq()            => List.Nil
        case scala.Seq(head, tail*) => List.Cons(head, tail.asScalus)
}

extension [A](self: scala.Option[A]) {

    /** Converts a [[scala.Option]] to an `Option` */
    @Ignore
    def asScalus: Option[A] = self match
        case scala.None    => Option.None
        case scala.Some(a) => Option.Some(a)
}
