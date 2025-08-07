package scalus.prelude

import scalus.{Compile, CompileDerivations, Ignore}
import scalus.builtin.Builtins.*
import scalus.builtin.{ByteString, Data, FromData, Pair, ToData}
import scalus.macros.Macros
import Ord.{<=>, Order}

import scala.annotation.{nowarn, tailrec}
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

/** * A typeclass for converting values of type `A` to a `String`.
  *
  * This is used to provide a string representation of values, which can be useful for debugging,
  * logging, or displaying information about the value.
  *
  * @tparam A
  *   the type of the value to be shown
  */
@FunctionalInterface
trait Show[A] extends (A => String) with CompileDerivations {
    override def apply(v: A): String
}

extension [A: Show](self: A) inline def show: String = summon[Show[A]].apply(self)

@Compile
object Show {
    // extension [A: Show](self: A) inline def show: String = summon[Show[A]].apply(self)

    given Show[Unit] = (x: Unit) => "()"
    given Show[ByteString] = (x: ByteString) =>
        appendString(appendString("\"", Prelude.encodeHex(x)), "\"")
    given Show[BigInt] = (x: BigInt) => Prelude.showBigInt(x)
    given Show[String] = (x: String) => appendString(appendString("\"", x), "\"")
    given Show[Boolean] = (x: Boolean) => if x then "True" else "False"

    given showList[T: Show]: Show[List[T]] = (xs: List[T]) => {
        @tailrec
        def go(start: String, rest: List[T]): String = rest match {
            case List.Nil => start
            case List.Cons(head, tail) =>
                go(appendString(appendString(start, head.show), ", "), tail)
        }
        appendString("[", appendString(go("", xs), "]"))
    }

    given Show[Data] = (x: Data) => {
        import scalus.builtin
        def showBuiltinList(xs: builtin.List[Data]): String = {
            if xs.isEmpty then ""
            else
                val head = xs.head.show
                if xs.tail.isEmpty then head
                else appendString(head, appendString(", ", showBuiltinList(xs.tail)))
        }
        val showConstr = () => {
            import scalus.builtin
            val p = unConstrData(x)
            val lst = appendString("[", appendString(showBuiltinList(p.snd), "]"))
            appendString("<", appendString(p.fst.show, appendString(", ", appendString(lst, ">"))))
        }

        val showMap = () => {
            import scalus.builtin
            val lst = unMapData(x)
            def showDataPair(x: Pair[Data, Data]): String = {
                val fstShow = x.fst.show
                val sndShow = x.snd.show
                appendString(appendString(fstShow, ": "), sndShow)
            }
            def go(xs: builtin.List[Pair[Data, Data]]): String = {
                if xs.isEmpty then ""
                else
                    val head = showDataPair(xs.head)
                    if xs.tail.isEmpty then head
                    else appendString(head, appendString(", ", go(xs.tail)))
            }
            appendString("{", appendString(go(lst), "}"))
        }
        val showList = () => {
            val lst = unListData(x)
            appendString("[", appendString(showBuiltinList(lst), "]"))
        }
        val showI = () => unIData(x).show
        val showB = () => unBData(x).show
        val f: () => String = chooseData(x, showConstr, showMap, showList, showI, showB)
        f()
    }
}

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
            if self.eqv(lhs, rhs) then Eq[B].eqv(mapper(lhs), mapper(rhs)) else false

    end extension

    given [A: Eq, B: Eq]: Eq[(A, B)] = (lhs: (A, B), rhs: (A, B)) =>
        lhs._1 === rhs._1 && lhs._2 === rhs._2

    given [A: Eq, B: Eq, C: Eq]: Eq[(A, B, C)] = (lhs: (A, B, C), rhs: (A, B, C)) =>
        lhs._1 === rhs._1 && lhs._2 === rhs._2 && lhs._3 === rhs._3

    def keyPairEq[A: Eq, B]: Eq[(A, B)] = (lhs: (A, B), rhs: (A, B)) => lhs._1 === rhs._1

end EqCompanion

extension [A](x: A)
    inline def ===(inline y: A)(using inline eq: Eq[A]): Boolean = eq(x, y)
    inline def !==(inline y: A)(using inline eq: Eq[A]): Boolean = !eq(x, y)

@FunctionalInterface
trait Ord[-A] extends Function2[A, A, Ord.Order] with scalus.CompileDerivations {
    override def apply(lhs: A, rhs: A): Ord.Order
}

@Compile
object Ord:
    inline def apply[A: Ord]: Ord[A] = summon[Ord[A]]

    enum Order:
        case Less, Equal, Greater

    import Order.*

    extension (self: Order)
        def isLess: Boolean = self match { case Less => true; case _ => false }
        def isLessEqual: Boolean = self match { case Greater => false; case _ => true }
        def isGreater: Boolean = self match { case Greater => true; case _ => false }
        def isGreaterEqual: Boolean = self match { case Less => false; case _ => true }
        def isEqual: Boolean = self match { case Equal => true; case _ => false }
        def nonEqual: Boolean = self match { case Equal => false; case _ => true }

        inline infix def ifEqualThen(inline other: => Order): Order =
            if self.nonEqual then self else other

    end extension

    given Eq[Order] = (lhs, rhs) =>
        lhs match
            case Less    => rhs.isLess
            case Greater => rhs.isGreater
            case Equal   => rhs.isEqual

    given Ord[Order] = (lhs, rhs) =>
        lhs match
            case Less    => if rhs.isLess then Equal else Less
            case Greater => if rhs.isGreater then Equal else Greater
            case Equal =>
                rhs match
                    case Less    => Greater
                    case Greater => Less
                    case Equal   => Equal

    extension [A: Ord](self: A)
        inline def <=>(inline other: A): Ord.Order = Ord[A].compare(self, other)
        def <(other: A): Boolean = (self <=> other).isLess
        def <=(other: A): Boolean = (self <=> other).isLessEqual
        def >(other: A): Boolean = (self <=> other).isGreater
        def >=(other: A): Boolean = (self <=> other).isGreaterEqual
        def equiv(other: A): Boolean = (self <=> other).isEqual
        def nonEquiv(other: A): Boolean = (self <=> other).nonEqual

    end extension

    def by[A, B: Ord](mapper: A => B): Ord[A] = (lhs: A, rhs: A) => mapper(lhs) <=> mapper(rhs)

    extension [A](self: Ord[A])
        inline def compare(inline lhs: A, inline rhs: A): Order = self.apply(lhs, rhs)

        def orElse(other: Ord[A]): Ord[A] = (lhs: A, rhs: A) =>
            self.compare(lhs, rhs) ifEqualThen other.compare(lhs, rhs)

        def orElseBy[B: Ord](mapper: A => B): Ord[A] = (lhs: A, rhs: A) =>
            self.compare(lhs, rhs) ifEqualThen Ord[B].compare(mapper(lhs), mapper(rhs))

    end extension

    given Ord[ByteString] = (x: ByteString, y: ByteString) =>
        if lessThanByteString(x, y) then Less
        else if equalsByteString(x, y) then Equal
        else Greater

    // TODO it's not truly overriding and doesn't work in generic function,
    //  see extension methods <, <=, >, >=, equiv, nonEquiv for [A: Ord]
    extension (self: ByteString)
        inline def <(other: ByteString): Boolean = lessThanByteString(self, other)
        inline def <=(other: ByteString): Boolean = lessThanEqualsByteString(self, other)
        inline def >(other: ByteString): Boolean = lessThanByteString(other, self)
        inline def >=(other: ByteString): Boolean = lessThanEqualsByteString(other, self)
        inline def equiv(other: ByteString): Boolean = equalsByteString(self, other)
        inline def nonEquiv(other: ByteString): Boolean = !equalsByteString(self, other)

    end extension

    given Ord[BigInt] = (x: BigInt, y: BigInt) =>
        if lessThanInteger(x, y) then Less else if lessThanInteger(y, x) then Greater else Equal

    given Ord[Boolean] = (x: Boolean, y: Boolean) =>
        if x then if y then Equal else Greater
        else if y then Less
        else Equal

    given Ord[Data] = (x: Data, y: Data) => {
        import scalus.builtin
        import Ord.Order.*

        def compareBuiltinList(xs: builtin.List[Data], ys: builtin.List[Data]): Order = {
            if xs.isEmpty && ys.isEmpty then Equal
            else if xs.isEmpty then Less
            else if ys.isEmpty then Greater
            else (xs.head <=> ys.head) ifEqualThen compareBuiltinList(xs.tail, ys.tail)
        }

        x match
            case Data.Constr(_, _) =>
                y match
                    case Data.Constr(_, _) =>
                        val px = unConstrData(x)
                        val py = unConstrData(y)
                        (px.fst <=> py.fst) ifEqualThen compareBuiltinList(px.snd, py.snd)
                    case _ => Less

            case Data.Map(_) =>
                y match
                    case Data.Constr(_, _) => Greater
                    case Data.Map(_) =>
                        val lstx = unMapData(x)
                        val lsty = unMapData(y)
                        def compareDataPair(px: Pair[Data, Data], py: Pair[Data, Data]): Order =
                            (px.fst <=> py.fst) ifEqualThen (px.snd <=> py.snd)

                        def go(
                            xs: builtin.List[Pair[Data, Data]],
                            ys: builtin.List[Pair[Data, Data]]
                        ): Order = {
                            if xs.isEmpty && ys.isEmpty then Equal
                            else if xs.isEmpty then Less
                            else if ys.isEmpty then Greater
                            else compareDataPair(xs.head, ys.head) ifEqualThen go(xs.tail, ys.tail)
                        }
                        go(lstx, lsty)
                    case _ => Less

            case Data.List(_) =>
                y match
                    case Data.Constr(_, _) => Greater
                    case Data.Map(_)       => Greater
                    case Data.List(_) =>
                        val lstx = unListData(x)
                        val lsty = unListData(y)
                        compareBuiltinList(lstx, lsty)
                    case _ => Less

            case Data.I(_) =>
                y match
                    case Data.Constr(_, _) => Greater
                    case Data.Map(_)       => Greater
                    case Data.List(_)      => Greater
                    case Data.I(_)         => unIData(x) <=> unIData(y)
                    case _                 => Less

            case Data.B(_) =>
                y match
                    case Data.B(_) => unBData(x) <=> unBData(y)
                    case _         => Greater
    }

    given [A: Ord, B: Ord]: Ord[(A, B)] = (lhs: (A, B), rhs: (A, B)) =>
        (lhs._1 <=> rhs._1) ifEqualThen (lhs._2 <=> rhs._2)

    given [A: Ord, B: Ord, C: Ord]: Ord[(A, B, C)] = (lhs: (A, B, C), rhs: (A, B, C)) =>
        (lhs._1 <=> rhs._1) ifEqualThen (lhs._2 <=> rhs._2) ifEqualThen (lhs._3 <=> rhs._3)

    def keyPairOrd[A: Ord, B]: Ord[(A, B)] = (lhs: (A, B), rhs: (A, B)) => lhs._1 <=> rhs._1

end Ord

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
