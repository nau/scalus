package scalus.prelude

import scalus.Compile
import scalus.Ignore
import scalus.builtin.Builtins.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.macros.Macros

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable
import scalus.cardano.onchain.{ImpossibleLedgerStateError, OnchainError, RequirementError}
import scalus.prelude.OrdCompanion.Order
import scalus.prelude.OrdCompanion.<=>

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
        inline def <=>(inline other: A): Order = summon[Ord[A]].compare(self, other)
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

/** Tests an expression, throwing an `IllegalArgumentException` if false.
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

enum List[+A]:
    case Nil extends List[Nothing]
    case Cons(head: A, tail: List[A]) extends List[A]

@Compile
object List:
    import Option.*

    /** Returns an empty list.
      *
      * This is an inline function that creates a new empty list of the specified type.
      *
      * @tparam A
      *   The type of elements this empty list would contain.
      * @return
      *   An empty list of type `List[A]`.
      */
    inline def empty[A]: List[A] = List.Nil

    /** Creates a list with a single element */
    def single[A](a: A): List[A] = Cons(a, List.Nil)

    /** Creates a list from a variable number of arguments.
      *
      * This method allows creating a list by simply providing the elements. Works only offchain.
      *
      * @tparam A
      *   The type of elements in the list.
      * @param args
      *   The elements to include in the list.
      * @return
      *   A list containing all provided arguments.
      * @example
      *   {{{
      *   List("a", "b", "c") === Cons("a", Cons("b", Cons("c", Nil)))
      *   List() === Nil
      *   }}}
      */
    @Ignore
    def apply[A](args: A*): List[A] = from(args)

    /** Creates a list from any Scala iterable collection.
      *
      * Works only offchain.
      *
      * @tparam A
      *   The type of elements in the list.
      * @param i
      *   The iterable collection to convert to a list.
      * @return
      *   A list containing all elements from the provided iterable.
      * @example
      *   {{{
      *   List.from(Vector(BigInt(1), BigInt(2), BigInt(3))) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   List.from(Iterator.empty) === Nil
      *   }}}
      */
    @Ignore
    def from[A](i: IterableOnce[A]): List[A] = i.iterator.foldRight(empty[A]) { case (a, b) =>
        Cons(a, b)
    }

    /** Creates a list from a Java iterable collection.
      *
      * Works only offchain.
      *
      * @tparam A
      *   The type of elements in the list.
      * @param i
      *   The Java iterable collection to convert to a list.
      * @return
      *   A list containing all elements from the provided Java iterable.
      * @example
      *   {{{
      *   import java.util.Arrays
      *   val javaList = Arrays.asList("a", "b", "c")
      *   List.from(javaList) === Cons("a", Cons("b", Cons("c", Nil)))
      *
      *   val emptyJavaList = new java.util.ArrayList[String]()
      *   List.from(emptyJavaList) === Nil
      *   }}}
      */
    @Ignore
    def from[A](i: java.lang.Iterable[A]): List[A] =
        import scala.jdk.CollectionConverters.*
        from(i.asScala)

    /** Creates a list containing a range of `BigInt` values, inclusive of both endpoints.
      *
      * This method creates a list containing all integers from `from` up to and including `to`. If
      * `from` is greater than `to`, an empty list is returned.
      *
      * @param from
      *   The starting value of the range (inclusive).
      * @param to
      *   The ending value of the range (inclusive).
      * @return
      *   A list containing all `BigInt` values from `from` to `to`, inclusive.
      * @example
      *   {{{
      *   List.range(1, 3) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   List.range(5, 3) === Nil
      *   List.range(0, 0) === Cons(BigInt(0), Nil)
      *   }}}
      */
    def range(from: BigInt, to: BigInt): List[BigInt] =
        if from <= to then Cons(from, range(from + 1, to))
        else Nil

    /** Creates a list containing a range of `BigInt` values, inclusive of the start but exclusive
      * of the end.
      *
      * This method creates a list containing all integers from `from` up to but not including `to`.
      * If `from` is greater than or equal to `to`, an empty list is returned.
      *
      * @param from
      *   The starting value of the range (inclusive).
      * @param to
      *   The ending value of the range (exclusive).
      * @return
      *   A list containing all `BigInt` values from `from` to `to-1`, inclusive.
      * @example
      *   {{{
      *   List.rangeUntil(1, 4) === Cons(BigInt(1), Cons(BigInt(2), Cons(BigInt(3), Nil)))
      *   List.rangeUntil(5, 5) === Nil
      *   List.rangeUntil(5, 3) === Nil
      *   }}}
      */
    def rangeUntil(from: BigInt, to: BigInt): List[BigInt] =
        if from < to then Cons(from, rangeUntil(from + 1, to))
        else Nil

    /** Creates a list by repeating a value a specified number of times.
      *
      * If `times` is less than or equal to 0, an empty list is returned.
      *
      * @param value
      *   The value to repeat in the list.
      * @param times
      *   The number of times to repeat the value.
      * @tparam A
      *   The type of the value to repeat.
      * @return
      *   A list containing the specified value repeated `times` times.
      * @example
      *   {{{
      *   List.fill("a", 3) === Cons("a", Cons("a", Cons("a", Nil)))
      *   List.fill(true, 0) === Nil
      *   List.fill("x", -1) === Nil
      *   }}}
      */
    def fill[A](value: A, times: BigInt): List[A] =
        if 0 < times then Cons(value, fill(value, times - 1))
        else Nil

    /** Combines two lists element-wise using the provided function.
      *
      * The resulting list will have the length of the shorter of the two input lists.
      *
      * @param a
      *   The first list.
      * @param b
      *   The second list.
      * @param f
      *   A function that takes one element from each list and produces a result.
      * @tparam A
      *   The element type of the first list.
      * @tparam B
      *   The element type of the second list.
      * @tparam C
      *   The element type of the resulting list.
      * @return
      *   A list containing the results of applying function `f` to corresponding elements of lists
      *   `a` and `b`.
      * @example
      *   {{{
      *   List.map2(List(BigInt(1), BigInt(2)), List(BigInt(3), BigInt(4)))(_ + _) === Cons(BigInt(4), Cons(BigInt(6), Nil))
      *   List.map2(List.empty[BigInt], List(BigInt(1), BigInt(2)))(_ + _) === Nil
      *   }}}
      */
    def map2[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
        a match
            case Cons(h1, t1) =>
                b match
                    case Cons(h2, t2) => Cons(f(h1, h2), map2(t1, t2)(f))
                    case Nil          => Nil
            case Nil => Nil

    given listToData[A: ToData]: ToData[scalus.prelude.List[A]] =
        (a: scalus.prelude.List[A]) => {
            def loop(a: scalus.prelude.List[A]): scalus.builtin.List[Data] =
                a match
                    case scalus.prelude.List.Nil => mkNilData()
                    case scalus.prelude.List.Cons(head, tail) =>
                        mkCons(summon[ToData[A]](head), loop(tail))

            listData(loop(a))
        }

    given ListFromData[A: FromData]: FromData[scalus.prelude.List[A]] = (d: Data) =>
        def loop(ls: scalus.builtin.List[Data]): scalus.prelude.List[A] =
            if ls.isEmpty then List.Nil
            else new List.Cons(fromData[A](ls.head), loop(ls.tail))
        loop(unListData(d))

    extension [A: Ord](self: List[A])
        def quicksort: List[A] =
            self match
                case List.Nil => List.Nil
                case List.Cons(head, tail) =>
                    val before = tail.filter { elem => (elem <=> head).isLess }.quicksort
                    val after = tail.filter { elem => !(elem <=> head).isLess }.quicksort
                    before ++ after.prepended(head)

    extension [A](self: List[A])
        inline def !!(idx: BigInt): A = self.at(idx)

        /** Checks if the list is empty.
          *
          * @return
          *   `true` if the list is empty, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].isEmpty === true
          *   Cons(1, Nil).isEmpty       === false
          *   }}}
          */
        def isEmpty: Boolean = self match
            case Nil        => true
            case Cons(_, _) => false

        /** Checks if the list is not empty.
          *
          * @return
          *   `true` if the list contains at least one element, `false` otherwise.
          * @example
          *   {{{
          *   List.empty[BigInt].nonEmpty === false
          *   Cons(1, Nil).nonEmpty       === true
          *   }}}
          */
        inline def nonEmpty: Boolean = !isEmpty

        def isDefinedAt(index: BigInt): Boolean = get(index).isDefined

        def at(index: BigInt): A = get(index).getOrFail("Index out of bounds")

        /** Retrieves the element at the specified index in the list.
          *
          * @param index
          *   The zero-based index of the element to retrieve.
          * @return
          *   An `Option` containing the element at the specified index, or `None` if the index is
          *   out of bounds.
          * @example
          *   {{{
          *   List.empty[BigInt].get(0) === None
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.get(1) === Some(2)
          *   list.get(3) === None
          *   list.get(-1) === None
          *   }}}
          */
        def get(index: BigInt): Option[A] = {
            if index < 0 then None
            else
                @tailrec
                def go(lst: List[A], currentIndex: BigInt): Option[A] = lst match
                    case Nil => None
                    case Cons(head, tail) =>
                        if currentIndex == index then Some(head)
                        else go(tail, currentIndex + 1)

                go(self, 0)
        }

        def contains[B >: A](elem: B)(using eq: Eq[B]): Boolean =
            find(_ === elem).isDefined

        /** Groups the elements of this list by the keys returned by the specified function.
          *
          * @param keyExtractor
          *   A function that extracts the key from each element.
          * @tparam K
          *   The type of the keys.
          * @return
          *   An `AssocMap` mapping each key to a list of elements that have that key.
          * @example
          *   {{{
          *   List.empty[BigInt].groupBy(_ % 2) === AssocMap.empty
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
          *   list.groupBy(_ % 2) === AssocMap.from((BigInt(1), Cons(1, Cons(3, Nil))), (BigInt(0), Cons(2, Cons(4, Nil))))
          *   }}}
          */
        def groupBy[K: Eq](keyExtractor: A => K): AssocMap[K, List[A]] =
            groupMap(keyExtractor)(identity)

        /** Groups the elements of this list by the keys returned by the key extractor function and
          * transforms each element using the value extractor function.
          *
          * @param keyExtractor
          *   A function that extracts the key from each element.
          * @param valueExtractor
          *   A function that transforms each element before collecting into groups.
          * @tparam K
          *   The type of the keys.
          * @tparam B
          *   The type of the transformed elements.
          * @return
          *   An `AssocMap` mapping each key to a list of transformed elements that have that key.
          * @example
          *   {{{
          *   List.empty[BigInt].groupMap(_ % 2)(_ * 2) === AssocMap.empty
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
          *   list.groupMap(_ % 2)(_ * 2) === AssocMap.from((BigInt(1), Cons(2, Cons(6, Nil))), (BigInt(0), Cons(4, Cons(8, Nil))))
          *   }}}
          */
        def groupMap[K: Eq, B](
            keyExtractor: A => K
        )(valueExtractor: A => B): AssocMap[K, List[B]] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, List[B]]): AssocMap[K, List[B]] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val key = keyExtractor(head)
                        val value = valueExtractor(head)
                        acc.get(key) match
                            case None =>
                                val newAcc = acc.insert(key, List.single(value))
                                go(tail, newAcc)
                            case Some(lst) =>
                                val newLst = lst.prepended(value)
                                val newAcc = acc.insert(key, newLst)
                                go(tail, newAcc)

            go(self, AssocMap.empty).mapValues { _.reverse }
        }

        /** Groups elements by the keys returned by the key extractor function, transforms each
          * element using the value extractor function, and combines values with the same key using
          * the reducer function.
          *
          * @param keyExtractor
          *   A function that extracts the key from each element.
          * @param valueExtractor
          *   A function that transforms each element before reduction.
          * @param reducer
          *   A function that combines two values with the same key.
          * @tparam K
          *   The type of the keys.
          * @tparam B
          *   The type of the transformed elements.
          * @return
          *   An `AssocMap` mapping each key to the reduced value of all elements with that key.
          * @example
          *   {{{
          *   List.empty[BigInt].groupMapReduce(_ % 2)(identity)(_ + _) === AssocMap.empty
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
          *   list.groupMapReduce(_ % 2)(identity)(_ + _) === AssocMap.from((BigInt(1), BigInt(4)), (BigInt(0), BigInt(6)))
          *   }}}
          */
        def groupMapReduce[K: Eq, B](
            keyExtractor: A => K
        )(valueExtractor: A => B)(reducer: (B, B) => B): AssocMap[K, B] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, B]): AssocMap[K, B] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val key = keyExtractor(head)
                        val value = valueExtractor(head)
                        acc.get(key) match
                            case None =>
                                val newAcc = acc.insert(key, value)
                                go(tail, newAcc)
                            case Some(oldValue) =>
                                val newValue = reducer(oldValue, value)
                                val newAcc = acc.insert(key, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty)
        }

        def zip[B](other: List[B]): List[(A, B)] = self match
            case Nil => Nil
            case Cons(selfHead, selfTail) =>
                other match
                    case Nil => Nil
                    case Cons(otherHead, otherTail) =>
                        Cons((selfHead, otherHead), selfTail.zip(otherTail))

        /** Adds an element at the beginning of this list */
        inline def prepended[B >: A](elem: B): List[B] = Cons(elem, self)
        inline def prependedAll[B >: A](other: List[B]): List[B] = other.appendedAll(self)

        def appended[B >: A](elem: B): List[B] = self match
            case Nil              => List.single(elem)
            case Cons(head, tail) => Cons(head, tail.appended(elem))

        inline def :+[B >: A](elem: B): List[B] = appended(elem)

        def appendedAll[B >: A](other: List[B]): List[B] = self match
            case Nil              => other
            case Cons(head, tail) => Cons(head, tail.appendedAll(other))

        inline def :++[B >: A](other: List[B]): List[B] = appendedAll(other)

        inline def concat[B >: A](other: List[B]): List[B] = appendedAll(other)
        inline def ++[B >: A](other: List[B]): List[B] = concat(other)

        /** Applies a function to each element of the list, producing a new list with the results.
          *
          * @param mapper
          *   A function that takes an element of type `A` and returns a value of type `B`.
          * @return
          *   A new list where each element is the result of applying `mapper` to the corresponding
          *   element of the original list.
          * @example
          *   {{{
          *   List.empty[BigInt].map(_ * 2) === Nil
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   val result = list.map(_ * 2)
          *   result === Cons(2, Cons(4, .Cons(6, Nil)))
          *   }}}
          */
        def map[B](mapper: A => B): List[B] =
            foldRight(List.empty[B]) { (head, tail) => Cons(mapper(head), tail) }

        def flatMap[B](mapper: A => List[B]): List[B] =
            foldRight(List.empty[B]) { (head, tail) => mapper(head) ++ tail }

        /** Filters the elements of the list based on a predicate.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element should
          *   be included in the resulting list, or `false` otherwise.
          * @return
          *   A new list containing only the elements that satisfy the predicate.
          * @example
          *   {{{
          *   List.empty[BigInt].filter(_ % 2 == 1) === Nil
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   val filtered = list.filter(_ % 2 == 1)
          *   filtered === Cons(1, Cons(3, Nil))
          *   }}}
          */
        def filter(predicate: A => Boolean): List[A] =
            foldRight(List.empty[A]) { (head, tail) =>
                if predicate(head) then Cons(head, tail) else tail
            }

        def filterNot(predicate: A => Boolean): List[A] = filter(!predicate(_))

        def filterMap[B](predicate: A => Option[B]): List[B] =
            foldRight(List.empty[B]) { (head, tail) =>
                predicate(head) match
                    case None        => tail
                    case Some(value) => Cons(value, tail)
            }

        /** Finds the first element in the list that satisfies the given predicate.
          *
          * @param predicate
          *   A function that takes an element of type `A` and returns `true` if the element matches
          *   the condition.
          * @return
          *   An `Option` containing the first element that satisfies the predicate, or `None` if no
          *   such element exists.
          * @example
          *   {{{
          *   List.empty[BigInt].find(_ > 1) === None
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.find(_ > 1) === Some(2)
          *   list.find(_ > 3) === None
          *   }}}
          */
        @tailrec
        def find(predicate: A => Boolean): Option[A] = self match
            case Nil              => None
            case Cons(head, tail) => if predicate(head) then Some(head) else tail.find(predicate)

        /** Finds the first element in the list that, when passed to the provided mapper function,
          * returns a `Some` value.
          *
          * @param mapper
          *   A function that takes an element of type `A` and returns an `Option[B]`.
          * @return
          *   An `Option[B]` containing the first non-`None` result from applying the mapper to the
          *   elements of the list, or `None` if no such element exists.
          * @example
          *   {{{
          *   List.empty[String].findMap(str => if str.length >= 3 then Some(str.length) else None) === None
          *
          *   val list: List[String] = Cons("a", Cons("bb", Cons("ccc", Nil)))
          *   list.findMap(str => if str.length >= 2 then Some(str.length) else None) === Some(2)
          *   list.findMap(str => if str.length >= 4 then Some(str.length) else None) === None
          *   }}}
          */
        @tailrec
        def findMap[B](mapper: A => Option[B]): Option[B] = self match
            case Nil => None
            case Cons(head, tail) =>
                val result = mapper(head)
                if result.isDefined then result else tail.findMap(mapper)

        /** Performs a left fold on the list.
          *
          * @param init
          *   The initial value to start the fold with.
          * @param combiner
          *   A function that combines the accumulated value and the current element.
          * @tparam B
          *   The type of the accumulated value.
          * @return
          *   The result of applying the combiner function to all elements of the list, starting
          *   with the initial value.
          * @example
          *   {{{
          *   List.empty[BigInt].foldLeft(BigInt(0))(_ + _) === BigInt(0)
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   val sum = list.foldLeft(BigInt(0))(_ + _)
          *   sum === BigInt(6)
          *   }}}
          */
        @tailrec
        def foldLeft[B](init: B)(combiner: (B, A) => B): B = self match
            case Nil              => init
            case Cons(head, tail) => tail.foldLeft(combiner(init, head))(combiner)

        def foldRight[B](init: B)(combiner: (A, B) => B): B = self match
            case Nil              => init
            case Cons(head, tail) => combiner(head, tail.foldRight(init)(combiner))

        def exists(predicate: A => Boolean): Boolean = find(predicate).isDefined

        @tailrec
        def forall(predicate: A => Boolean): Boolean = self match
            case Nil              => true
            case Cons(head, tail) => if predicate(head) then tail.forall(predicate) else false

        def count(p: A => Boolean): BigInt = foldLeft(BigInt(0)) { (counter, elem) =>
            if p(elem) then counter + 1 else counter
        }

        def indexOf[B >: A](elem: B)(using eq: Eq[B]): BigInt = indexOfOption(elem).getOrElse(-1)

        /** Finds the index of the first occurrence of the specified element in the list.
          *
          * @param elem
          *   The element to search for in the list.
          * @tparam B
          *   The type of the element being searched for, which must be a supertype of `A`.
          * @return
          *   An `Option` containing the index of the first occurrence of the element, or `None` if
          *   the element is not found.
          * @example
          *   {{{
          *   List.empty[BigInt].indexOfOption(BigInt(2)) === None
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.indexOfOption(BigInt(2)) === Some(BigInt(1))
          *   list.indexOfOption(BigInt(4)) === None
          *   }}}
          */
        def indexOfOption[B >: A](elem: B)(using eq: Eq[B]): Option[BigInt] = {
            @tailrec
            def go(lst: List[A], index: BigInt): Option[BigInt] = lst match
                case Nil => None
                case Cons(head, tail) =>
                    if head === elem then Some(index) else go(tail, index + 1)

            go(self, BigInt(0))
        }

        def last: A = lastOption.getOrFail("last of empty list")

        /** Returns the last element of the list.
          *
          * @return
          *   An `Option` containing the last element of the list, or `None` if the list is empty.
          * @example
          *   {{{
          *   List.empty[BigInt].lastOption === None
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.lastOption === Some(3)
          *   }}}
          */
        @tailrec
        def lastOption: Option[A] = self match
            case Nil               => None
            case Cons(value, tail) => if tail.isEmpty then Some(value) else tail.lastOption

        /** Returns the number of elements in the list.
          *
          * @return
          *   The number of elements in the list as a `BigInt`.
          * @example
          *   {{{
          *   List.empty[BigInt].length === BigInt(0)
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.length === BigInt(3)
          *   }}}
          */
        def length: BigInt = foldLeft(BigInt(0)) { (counter, _) => counter + 1 }

        /** Alias for `length`.
          *
          * @return
          *   The number of elements in the list as a `BigInt`.
          */
        inline def size: BigInt = length

        /** Returns the first element of the list.
          *
          * @return
          *   The first element of the list.
          * @throws NoSuchElementException
          *   If the list is empty.
          * @example
          *   {{{
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.head === BigInt(1)
          *   // List.empty[BigInt].head would throw NoSuchElementException
          *   }}}
          */
        def head: A = headOption.getOrFail("head of empty list")

        /** Returns the first element of the list as an [[Option]].
          *
          * @return
          *   An `Option` containing the first element of the list, or `None` if the list is empty.
          * @example
          *   {{{
          *   List.empty[BigInt].headOption === None
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.headOption === Some(1)
          *   }}}
          */
        def headOption: Option[A] = self match
            case Nil            => None
            case Cons(value, _) => Some(value)

        /** Returns a list consisting of all elements except the first.
          *
          * @return
          *   A list containing all elements except the first.
          * @throws NoSuchElementException
          *   If the list is empty.
          * @example
          *   {{{
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.tail === Cons(2, Cons(3, Nil))
          *   // List.empty[BigInt].tail would throw NoSuchElementException
          *   }}}
          */
        def tail: List[A] = self match
            case Nil           => throw new NoSuchElementException("tail of empty list")
            case Cons(_, rest) => rest

        @tailrec
        def drop(skip: BigInt): List[A] =
            if skip <= 0 then self
            else
                self match
                    case Nil           => Nil
                    case Cons(_, tail) => tail.drop(skip - 1)

        def dropRight(skip: BigInt): List[A] =
            if skip <= 0 then self
            else
                self.foldRight((List.empty[A], skip)) { (head, acc) =>
                    if acc._2 > 0 then (Nil, acc._2 - 1)
                    else (Cons(head, acc._1), acc._2)
                }._1

        def deleteFirst[B >: A](elem: B)(using eq: Eq[B]): List[A] = {
            def go(lst: List[A]): List[A] = lst match
                case Nil => Nil
                case Cons(head, tail) =>
                    if head === elem then tail
                    else Cons(head, go(tail))

            go(self)
        }

        def take(count: BigInt): List[A] =
            if count <= 0 then Nil
            else
                self match
                    case Nil              => Nil
                    case Cons(head, tail) => Cons(head, tail.take(count - 1))

        def takeRight(count: BigInt): List[A] =
            if count <= 0 then Nil
            else
                self.foldRight((List.empty[A], BigInt(0))) { (head, acc) =>
                    if acc._2 > count then acc
                    else (Cons(head, acc._1), acc._2 + 1)
                }._1

        def takeWhile(predicate: A => Boolean): List[A] = self match
            case Nil => Nil
            case Cons(head, tail) =>
                if predicate(head) then Cons(head, tail.takeWhile(predicate))
                else Nil

        def unique[B >: A](using eq: Eq[B]): List[A] =
            foldLeft(List.empty[A]) { (acc, elem) =>
                if acc.exists(_ === elem) then acc
                else Cons(elem, acc)
            }.reverse

        @tailrec
        def difference[B >: A](other: List[B])(using eq: Eq[B]): List[A] = {
            if self.isEmpty then Nil
            else
                other match
                    case Nil => self
                    case Cons(head, tail) =>
                        (self.deleteFirst(head): List[A]).difference(tail)
        }

        inline def init: List[A] = dropRight(1)

        /** Returns a new list with elements in reverse order.
          *
          * @return
          *   A new list containing the same elements but in reverse order.
          * @example
          *   {{{
          *   List.empty[BigInt].reverse === Nil
          *
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.reverse === Cons(3, Cons(2, Cons(1, Nil)))
          *   }}}
          */
        def reverse: List[A] = foldLeft(List.empty[A]) { (acc, elem) => Cons(elem, acc) }

        /** Applies the given function to each element of the list.
          *
          * @param f
          *   The function to apply to each element.
          * @example
          *   {{{
          *   var sum = BigInt(0)
          *   val list: List[BigInt] = Cons(1, Cons(2, Cons(3, Nil)))
          *   list.foreach(elem => sum += elem)
          *   sum === BigInt(6)
          *   }}}
          */
        @tailrec
        def foreach(f: A => Unit): Unit = self match
            case Nil              => ()
            case Cons(head, tail) => f(head); tail.foreach(f)

        /** Converts to a [[scala.Seq]] */
        @Ignore
        def asScala: scala.Seq[A] =
            val buf = mutable.ListBuffer.empty[A]
            for e <- self do buf.addOne(e)
            buf.toList

    extension [A](elem: A) inline def +:[B >: A](l: List[B]): List[B] = l.prepended(elem)
    extension [A](left: List[A])
        inline def ++:[B >: A](other: List[B]): List[B] = other.prependedAll(left)

    extension [A](self: scala.Seq[A])
        /** Converts a [[scala.Seq]] to a `List` */
        @Ignore
        def asScalus: List[A] = self match
            case scala.Seq()            => Nil
            case scala.Seq(head, tail*) => Cons(head, tail.asScalus)

    given listEq[A: Eq]: Eq[List[A]] = (lhs: List[A], rhs: List[A]) =>
        lhs match
            case Nil =>
                rhs match
                    case Nil        => true
                    case Cons(_, _) => false
            case Cons(headLhs, tailLhs) =>
                rhs match
                    case Nil                    => false
                    case Cons(headRhs, tailRhs) => headLhs === headRhs && tailLhs === tailRhs

    given listOrd[A: Ord]: Ord[List[A]] = (lhs: List[A], rhs: List[A]) =>
        lhs match
            case Nil =>
                rhs match
                    case Nil        => Order.Equal
                    case Cons(_, _) => Order.Less
            case Cons(headLhs, tailLhs) =>
                rhs match
                    case Nil => Order.Greater
                    case Cons(headRhs, tailRhs) =>
                        val order = headLhs <=> headRhs
                        if order.nonEqual then order else tailLhs <=> tailRhs

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

/** Alternative to `scala.Option` in onchain code.
  * @tparam A
  */
enum Option[+A]:
    // note, that order of cases matters, as it is used in serialization
    case Some(value: A) extends Option[A]
    case None extends Option[Nothing]

@Compile
object Option {

    /** Constructs an `Option` from a value. If the value is `null`, it returns `None`, otherwise
      * `Some(value)`.
      */
    @Ignore
    inline def apply[A](x: A): Option[A] = if x == null then None else Some(x)

    inline def empty[A]: Option[A] = None

    extension [A](self: Option[A])
        def isEmpty: Boolean = self match
            case None    => true
            case Some(_) => false

        inline def nonEmpty: Boolean = !isEmpty

        inline def isDefined: Boolean = nonEmpty

        inline def getOrFail(inline message: String = "None.getOrFail"): A = self match
            case None        => throw new NoSuchElementException(message)
            case Some(value) => value

        inline infix def orFail(inline message: String = "None.orFail"): Unit = self match
            case None    => throw new NoSuchElementException(message)
            case Some(_) => ()

        def get: A = getOrFail("None.get")

        def getOrElse[B >: A](default: B): B = self match
            case None    => default
            case Some(a) => a

        def orElse[B >: A](alternative: Option[B]): Option[B] = self match
            case None    => alternative
            case Some(a) => self

        /** Converts an `Option` to a [[scala.Option]] */
        @Ignore
        def asScala: scala.Option[A] = self match
            case None    => scala.None
            case Some(a) => scala.Some(a)

        def map[B](mapper: A => B): Option[B] = self match
            case None    => None
            case Some(a) => Some(mapper(a))

        def flatMap[B](mapper: A => Option[B]): Option[B] = self match
            case None    => None
            case Some(a) => mapper(a)

        def filter(predicate: A => Boolean): Option[A] = self match
            case None    => None
            case Some(a) => if predicate(a) then self else None

        def filterNot(predicate: A => Boolean): Option[A] = filter(!predicate(_))

        def contains[B >: A](elem: B)(using eq: Eq[B]): Boolean = self match
            case None    => false
            case Some(a) => a === elem

        def exists(p: A => Boolean): Boolean = self match
            case None    => false
            case Some(a) => p(a)

        def forall(p: A => Boolean): Boolean = self match
            case None    => true
            case Some(a) => p(a)

        inline def find(p: A => Boolean): Option[A] = filter(p)

    extension [A](self: Option[Option[A]])
        def flatten: Option[A] = self match
            case None    => None
            case Some(a) => a

    extension [A](self: scala.Option[A])
        /** Converts a [[scala.Option]] to an `Option` */
        @Ignore
        // TODO maybe rename to asScalusOption
        def asScalus: Option[A] = self match
            case scala.None    => None
            case scala.Some(a) => Some(a)

    given optionEq[A](using eq: Eq[A]): Eq[Option[A]] = (a: Option[A], b: Option[A]) =>
        a match
            case None =>
                b match
                    case None    => true
                    case Some(_) => false
            case Some(value) =>
                b match
                    case None         => false
                    case Some(value2) => value === value2

    given optionFromData[A: FromData]: FromData[Option[A]] = (d: Data) =>
        val pair = unConstrData(d)
        if pair.fst == BigInt(0) then new Option.Some(fromData[A](pair.snd.head))
        else Option.None

    given optionToData[A: ToData]: ToData[Option[A]] =
        (a: Option[A]) => {
            a match {
                case Option.Some(v) =>
                    constrData(0, mkCons(v.toData, mkNilData()))
                case Option.None => constrData(1, mkNilData())
            }
        }

}

enum These[+A, +B]:
    case This(a: A)
    case That(b: B)
    case These(a: A, b: B)

case class AssocMap[A, B](toList: List[(A, B)])

@Compile
object AssocMap {
    import List.*
    import Option.*

    def empty[A, B]: AssocMap[A, B] = AssocMap(List.empty[(A, B)])
    def singleton[A, B](key: A, value: B): AssocMap[A, B] = AssocMap(List.single((key, value)))
    inline def unsafeFromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = AssocMap(lst)

    def fromList[A: Eq, B](lst: List[(A, B)]): AssocMap[A, B] = AssocMap(
      lst.foldLeft(List.empty) { (acc, elem) =>
          if acc.exists(_._1 === elem._1) then acc
          else Cons(elem, acc)
      }
    )

    given AssocMapFromData[A: FromData: Eq, B: FromData]: FromData[AssocMap[A, B]] =
        (d: Data) =>
            def loop(
                ls: scalus.builtin.List[scalus.builtin.Pair[Data, Data]]
            ): scalus.prelude.List[(A, B)] =
                if ls.isEmpty then Nil
                else
                    val pair = ls.head
                    Cons(
                      (fromData[A](pair.fst), fromData[B](pair.snd)),
                      loop(ls.tail)
                    )
            AssocMap.unsafeFromList(loop(unMapData(d)))

    given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] =
        (a: AssocMap[A, B]) => {
            def go(a: List[(A, B)]): scalus.builtin.List[scalus.builtin.Pair[Data, Data]] =
                a match {
                    case Nil => mkNilPairData()
                    case Cons(tuple, tail) =>
                        tuple match {
                            case (a, b) =>
                                mkCons(
                                  scalus.builtin.Pair(summon[ToData[A]](a), summon[ToData[B]](b)),
                                  go(tail)
                                )
                        }
                }

            mapData(go(a.toList))
        }

    extension [A, B](self: AssocMap[A, B])
        inline def isEmpty: Boolean = self.toList.isEmpty
        inline def nonEmpty: Boolean = self.toList.nonEmpty
        inline def length: BigInt = self.toList.length
        inline def size: BigInt = length
        def keys: List[A] = self.toList.map { case (k, _) => k }
        def values: List[B] = self.toList.map { case (_, v) => v }
        def mapValues[C](f: B => C): AssocMap[A, C] = AssocMap(self.toList.map((k, v) => (k, f(v))))
        def forall(f: ((A, B)) => Boolean): Boolean = self.toList.forall(f)
        def exists(f: ((A, B)) => Boolean): Boolean = self.toList.exists(f)

        def filterKeys(predicate: A => Boolean): AssocMap[A, B] = AssocMap(self.toList.filter {
            case (k, _) => predicate(k)
        })

        def filter(predicate: ((A, B)) => Boolean): AssocMap[A, B] =
            AssocMap(self.toList.filter(predicate))

        def filterNot(predicate: ((A, B)) => Boolean): AssocMap[A, B] =
            AssocMap(self.toList.filterNot(predicate))

        def find(predicate: ((A, B)) => Boolean): Option[(A, B)] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[(A, B)] = lst match
                case Nil => None
                case Cons(pair, tail) =>
                    if predicate(pair) then Some(pair) else go(tail)

            go(self.toList)
        }

        inline def findOrFail(
            predicate: ((A, B)) => Boolean,
            inline message: String = "None.findOrFail"
        ): (A, B) =
            find(predicate).getOrFail(message)

        def foldLeft[C](init: C)(combiner: (C, (A, B)) => C): C =
            self.toList.foldLeft(init) { (acc, pair) => combiner(acc, pair) }

        def foldRight[C](init: C)(combiner: ((A, B), C) => C): C =
            self.toList.foldRight(init) { (pair, acc) => combiner(pair, acc) }

    extension [A: Eq, B](self: AssocMap[A, B])
        /** Optionally returns the value associated with a key.
          *
          * @param key
          *   the key value
          * @return
          *   an option value containing the value associated with `key` in this map, or `None` if
          *   none exists.
          */
        def get(key: A): Option[B] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[B] = lst match
                case Nil => None
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) => if k === key then Some(v) else go(tail)

            go(self.toList)
        }

        inline def getOrFail(key: A, inline message: String = "None.getOrFail"): B =
            get(key).getOrFail(message)

        def contains(key: A): Boolean = get(key).isDefined

        @deprecated("Use `get` instead")
        def lookup(key: A): Option[B] = get(key)

        def insert(key: A, value: B): AssocMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil => Cons((key, value), Nil)
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            if k === key then Cons((key, value), tail)
                            else Cons(pair, go(tail))

            AssocMap(go(self.toList))
        }

        def delete(key: A): AssocMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil => Nil
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            if k === key then tail else Cons(pair, go(tail))

            AssocMap(go(self.toList))
        }

    def union[A: Eq, B, C](
        lhs: AssocMap[A, B],
        rhs: AssocMap[A, C]
    ): AssocMap[A, These[B, C]] = {
        def go(lst: List[(A, B)]): List[(A, These[B, C])] = lst match
            case Nil => Nil
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        val optionR = rhs.get(k)
                        val these = optionR match
                            case None    => These.This(v)
                            case Some(r) => These.These(v, r)
                        Cons((k, these), go(tail))

        val lhs1 = go(lhs.toList) // all left with corresponding right

        val rhsNotInLhs =
            rhs.toList.filter { case (a, c) => !lhs.toList.exists(p => p._1 === a) }

        val rhsThat = rhsNotInLhs.map { case (k, v) => (k, These.That(v)) }
        AssocMap(lhs1.appendedAll(rhsThat))
    }

    given assocMapEq[A: Eq, B: Eq]: Eq[AssocMap[A, B]] =
        (lhs: AssocMap[A, B], rhs: AssocMap[A, B]) =>
            lhs.toList.length === rhs.toList.length && lhs.toList.forall { case (key, lhsValue) =>
                rhs.get(key) match
                    case None           => false
                    case Some(rhsValue) => lhsValue === rhsValue
            }
}

/** Alternative to `scala.collection.immutable.SortedMap` in onchain code.
  * @tparam A
  *   the type of keys, must be an instance of `Ord`
  * @tparam B
  *   the type of values
  */
case class SortedMap[A, B] private (toList: List[(A, B)])

@Compile
object SortedMap {
    import List.*
    import Option.*

    /** Constructs an empty `SortedMap`.
      *
      * @example
      *   {{{
      *   SortedMap.empty.toList === List.empty
      *   }}}
      */
    def empty[A, B]: SortedMap[A, B] = SortedMap(List.empty[(A, B)])

    /** Constructs a `SortedMap` with a single key-value pair.
      *
      * @param key
      *   the key to insert
      * @param value
      *   the value associated with the key
      * @return
      *   a `SortedMap` containing the single key-value pair
      * @example
      *   {{{
      *   SortedMap.singleton("key", "value").toList === List.single(("key", "value"))
      *   }}}
      */
    def singleton[A, B](key: A, value: B): SortedMap[A, B] = SortedMap(
      List.single((key, value))
    )

    /** Constructs a `SortedMap` in unsafe way from a list of key-value pairs assuming that it's in
      * strictly ascending order without any validation.
      *
      * @param lst
      *   the list of key-value pairs
      * @return
      *   a `SortedMap` containing the key-value pairs from the list
      * @example
      *   {{{
      *   SortedMap.unsafeFromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
      *   }}}
      */
    inline def unsafeFromList[A, B](lst: List[(A, B)]): SortedMap[A, B] = SortedMap(lst)

    /** Constructs a `SortedMap` from a list of key-value pairs, ordering it in strictly ascending
      * order, in case when a key is presented multiple times, the first occurrence prevails.
      *
      * @param lst
      *   the list of key-value pairs
      * @return
      *   a `SortedMap` containing the key-value pairs from the list, with unique keys in sorted
      *   order
      * @example
      *   {{{
      *   SortedMap.fromList(List.Cons(("b", 2), List.Cons(("a", 1), List.Nil))).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
      *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("a", 2), List.Nil))).toList === List.Cons(("a", 1), List.Nil)
      *   }}}
      */
    def fromList[A: Ord, B](lst: List[(A, B)]): SortedMap[A, B] = {
        def insertIfDoesNotExist(lst: List[(A, B)], key: A, value: B): List[(A, B)] = lst match
            case Nil => single(key, value)
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        key <=> k match
                            case Order.Less    => Cons((key, value), lst)
                            case Order.Greater => Cons(pair, insertIfDoesNotExist(tail, key, value))
                            case Order.Equal   => lst

        SortedMap(
          lst.foldLeft(List.empty) { (acc, pair) => insertIfDoesNotExist(acc, pair._1, pair._2) }
        )
    }

    /** Constructs a `SortedMap` from a list of key-value pairs, or fails if the list is not in
      * strictly ascending order.
      *
      * @param lst
      *   the list of key-value pairs
      * @return
      *   a `SortedMap` containing the key-value pairs from the list, or fails if the list is not in
      *   strictly ascending order
      * @throws OnchainError
      *   if the list is not in strictly ascending order
      * @example
      *   {{{
      *   SortedMap.fromStrictlyAscendingList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
      *   SortedMap.fromStrictlyAscendingList(List.Cons(("a", 1), List.Cons(("a", 2), List.Nil))) // throws OnchainError
      *   }}}
      */
    def fromStrictlyAscendingList[A: Ord, B](
        lst: List[(A, B)]
    ): SortedMap[A, B] = {
        @tailrec
        def checkStrictlyAscendingOrder(
            lst: List[(A, B)]
        ): Boolean = lst match
            case Nil => true
            case Cons(pair1, tail) =>
                tail match
                    case Nil => true
                    case Cons(pair2, _) =>
                        pair1._1 <=> pair2._1 match
                            case Order.Less => checkStrictlyAscendingOrder(tail)
                            case _          => false

        if checkStrictlyAscendingOrder(lst) then SortedMap(lst)
        else fail("List is not strictly ascending")
    }

    /** Merges two `SortedMap`s into a new `SortedMap` containing keys from both maps. if a key is
      * only present in left map, it is wrapped in `These.This`, if only in right map, it is wrapped
      * in `These.That`, if a key is present in both maps, its values are wrapped in `These.These`.
      * The resulting map is sorted by keys in strictly ascending order.
      *
      * This method is useful for combining two maps where you want to keep track of which keys are
      * unique to each map and which keys are shared between
      *
      * @param lhs
      *   the left-hand side `SortedMap`
      * @param rhs
      *   the right-hand side `SortedMap`
      * @return
      *   a new `SortedMap` containing keys from both maps, with values combined into `These`
      * @example
      *   {{{
      *   val map1 = SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil)))
      *   val map2 = SortedMap.fromList(List.Cons(("b", 3), List.Cons(("c", 4), List.Nil)))
      *   SortedMap.union(map1, map2).toList === List.Cons(("a", These.This(1)), List.Cons(("b", These.These(2, 3)), List.Cons(("c", These.That(4)), List.Nil)))
      *   }}}
      */
    def union[A: Ord, B, C](
        lhs: SortedMap[A, B],
        rhs: SortedMap[A, C]
    ): SortedMap[A, These[B, C]] = {
        def go(
            lhs: List[(A, B)],
            rhs: List[(A, C)]
        ): List[(A, These[B, C])] = lhs match
            case Nil =>
                rhs match
                    case Nil => Nil
                    case Cons(rhsPair, rhsTail) =>
                        Cons(
                          (rhsPair._1, These.That(rhsPair._2)),
                          rhsTail.map { pair => (pair._1, These.That(pair._2)) }
                        )
            case Cons(lhsPair, lhsTail) =>
                rhs match
                    case Nil =>
                        Cons(
                          (lhsPair._1, These.This(lhsPair._2)),
                          lhsTail.map { pair => (pair._1, These.This(pair._2)) }
                        )
                    case Cons(rhsPair, rhsTail) =>
                        lhsPair match
                            case (lhsKey, lhsValue) =>
                                rhsPair match
                                    case (rhsKey, rhsValue) =>
                                        lhsKey <=> rhsKey match
                                            case Order.Less =>
                                                Cons(
                                                  (lhsKey, These.This(lhsValue)),
                                                  go(lhsTail, rhs)
                                                )
                                            case Order.Greater =>
                                                Cons(
                                                  (rhsKey, These.That(rhsValue)),
                                                  go(lhs, rhsTail)
                                                )
                                            case Order.Equal =>
                                                Cons(
                                                  (lhsKey, These.These(lhsValue, rhsValue)),
                                                  go(lhsTail, rhsTail)
                                                )

        SortedMap(go(lhs.toList, rhs.toList))
    }

    /** Provides an `Eq` instance for `SortedMap[A, B]` where both key and value types are instances
      * of `Eq`.
      */
    given sortedMapEq[A: Eq, B: Eq]: Eq[SortedMap[A, B]] =
        (lhs: SortedMap[A, B], rhs: SortedMap[A, B]) =>
            import Eq.given
            lhs.toList === rhs.toList

    /** Provides an `Ord` instance for `SortedMap[A, B]` where both key and value types are
      * instances of `Ord`.
      */
    given sortedMapOrd[A: Ord, B: Ord]: Ord[SortedMap[A, B]] =
        (lhs: SortedMap[A, B], rhs: SortedMap[A, B]) =>
            import Ord.given
            lhs.toList <=> rhs.toList

    /** Provides a `FromData` instance for `SortedMap[A, B]` where both key and value types are
      * instances of `FromData`.
      */
    given sortedMapFromData[A: FromData, B: FromData]: FromData[SortedMap[A, B]] =
        (d: Data) =>
            def loop(
                ls: scalus.builtin.List[scalus.builtin.Pair[Data, Data]]
            ): scalus.prelude.List[(A, B)] =
                if ls.isEmpty then Nil
                else
                    val pair = ls.head
                    Cons(
                      (fromData[A](pair.fst), fromData[B](pair.snd)),
                      loop(ls.tail)
                    )
            SortedMap(loop(unMapData(d)))

    /** Provides a `ToData` instance for `SortedMap[A, B]` where both key and value types are
      * instances of `ToData`.
      */
    given sortedMapToData[A: ToData, B: ToData]: ToData[SortedMap[A, B]] =
        (a: SortedMap[A, B]) => {
            def go(a: List[(A, B)]): scalus.builtin.List[scalus.builtin.Pair[Data, Data]] =
                a match {
                    case Nil => mkNilPairData()
                    case Cons(tuple, tail) =>
                        tuple match {
                            case (a, b) =>
                                mkCons(
                                  scalus.builtin.Pair(summon[ToData[A]](a), summon[ToData[B]](b)),
                                  go(tail)
                                )
                        }
                }

            mapData(go(a.toList))
        }

    extension [A, B](self: SortedMap[A, B])
        /** Checks if the `SortedMap` is empty.
          *
          * @return
          *   `true` if the map is empty, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.isEmpty === true
          *   SortedMap.singleton("key", "value").isEmpty === false
          *   }}}
          */
        inline def isEmpty: Boolean = self.toList.isEmpty

        /** Checks if the `SortedMap` is non-empty.
          *
          * @return
          *   `true` if the map is non-empty, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.nonEmpty === false
          *   SortedMap.singleton("key", "value").nonEmpty === true
          *   }}}
          */
        inline def nonEmpty: Boolean = self.toList.nonEmpty

        /** Returns the number of key-value pairs in the `SortedMap`.
          *
          * @return
          *   the number of key-value pairs in the map
          * @example
          *   {{{
          *   SortedMap.empty.length === 0
          *   SortedMap.singleton("key", "value").length === 1
          *   }}}
          */
        inline def length: BigInt = self.toList.length

        /** Returns the size of the `SortedMap`, which is the same as its length.
          *
          * @return
          *   the size of the map
          * @example
          *   {{{
          *   SortedMap.empty.size === 0
          *   SortedMap.singleton("key", "value").size === 1
          *   }}}
          */
        inline def size: BigInt = length

        /** Returns a list of keys in the `SortedMap`.
          *
          * @return
          *   a list containing all keys in the map
          * @example
          *   {{{
          *   SortedMap.empty.keys === List.empty
          *   SortedMap.singleton("key", "value").keys === List.single("key")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).keys === List.Cons("a", List.Cons("b", List.Nil))
          *   }}}
          */
        def keys: List[A] = self.toList.map { case (k, _) => k }

        /** Returns a list of values in the `SortedMap`.
          *
          * @return
          *   a list containing all values in the map
          * @example
          *   {{{
          *   SortedMap.empty.values === List.empty
          *   SortedMap.singleton("key", "value").values === List.single("value")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).values === List.Cons(1, List.Cons(2, List.Nil))
          *   }}}
          */
        def values: List[B] = self.toList.map { case (_, v) => v }

        /** Checks if a predicate holds for all key-value pairs in the `SortedMap`.
          *
          * @param f
          *   the predicate function to check
          * @return
          *   `true` if the predicate holds for all pairs, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.forall(_ => true) === true
          *   SortedMap.singleton("key", "value").forall(_._1 === "foo") === false
          *   SortedMap.singleton("key", "value").forall(_._1 === "key") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).forall(_._2 > 0) === true
          *   }}}
          */
        def forall(f: ((A, B)) => Boolean): Boolean = self.toList.forall(f)

        /** Checks if a predicate holds for at least one key-value pair in the `SortedMap`.
          *
          * @param f
          *   the predicate function to check
          * @return
          *   `true` if the predicate holds for at least one pair, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.exists(_ => true) === false
          *   SortedMap.singleton("key", "value").exists(_._1 === "foo") === false
          *   SortedMap.singleton("key", "value").exists(_._1 === "key") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).exists(_._2 > 1) === true
          *   }}}
          */
        def exists(f: ((A, B)) => Boolean): Boolean = self.toList.exists(f)

        /** Maps the values of the `SortedMap` using a function.
          *
          * @param f
          *   the function to apply to each value
          * @return
          *   a new `SortedMap` with the same keys and transformed values
          * @example
          *   {{{
          *   SortedMap.singleton("key", 1).mapValues(_ + 1).toList === List.single(("key", 2))
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).mapValues(_ * 2).toList === List.Cons(("a", 2), List.Cons(("b", 4), List.Nil))
          *   }}}
          */
        def mapValues[C](f: B => C): SortedMap[A, C] = SortedMap(
          self.toList.map((k, v) => (k, f(v)))
        )

        /** Filters the keys of the `SortedMap` based on a predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key
          * @return
          *   a new `SortedMap` containing only the key-value pairs where the key satisfies the
          *   predicate
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).filterKeys(_ === "a").toList === List.Cons(("a", 1), List.Nil)
          *   }}}
          */
        def filterKeys(predicate: A => Boolean): SortedMap[A, B] =
            SortedMap(self.toList.filter { case (k, _) =>
                predicate(k)
            })

        /** Filters the key-value pairs of the `SortedMap` based on a predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair
          * @return
          *   a new `SortedMap` containing only the key-value pairs that satisfy the predicate
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).filter(_._2 > 1).toList === List.Cons(("b", 2), List.Nil)
          *   }}}
          */
        def filter(predicate: ((A, B)) => Boolean): SortedMap[A, B] =
            SortedMap(self.toList.filter(predicate))

        /** Filters the key-value pairs of the `SortedMap` based on a negated predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair, negated
          * @return
          *   a new `SortedMap` containing only the key-value pairs that do not satisfy the
          *   predicate
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).filterNot(_._2 > 1).toList === List.Cons(("a", 1), List.Nil)
          *   }}}
          */
        def filterNot(predicate: ((A, B)) => Boolean): SortedMap[A, B] =
            SortedMap(self.toList.filterNot(predicate))

        /** Optionally returns the first key-value pair that satisfies a predicate.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair
          * @return
          *   an `Option` containing the first key-value pair that satisfies the predicate, or
          *   `None` if no such pair exists
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).find(_._1 === "b") === Some(("b", 2))
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).find(_._1 === "c") === None
          *   }}}
          */
        def find(predicate: ((A, B)) => Boolean): Option[(A, B)] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[(A, B)] = lst match
                case Nil => None
                case Cons(pair, tail) =>
                    if predicate(pair) then Some(pair) else go(tail)

            go(self.toList)
        }

        /** Finds the first key-value pair that satisfies a predicate, or fails with a message if no
          * such pair exists.
          *
          * @param predicate
          *   the predicate function to apply to each key-value pair
          * @param message
          *   the error message to use if no pair is found
          * @return
          *   the first key-value pair that satisfies the predicate
          * @throws NoSuchElementException
          *   if no such pair exists
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).findOrFail(_._1 === "b") === ("b", 2)
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).findOrFail(_._1 === "c", "No pair found") // throws NoSuchElementException with message "No pair found"
          *   }}}
          */
        inline def findOrFail(
            predicate: ((A, B)) => Boolean,
            inline message: String = "None.findOrFail"
        ): (A, B) =
            find(predicate).getOrFail(message)

        /** Folds the `SortedMap` from the left, combining key-value pairs into a single value.
          *
          * @param init
          *   the initial value to start folding from
          * @param combiner
          *   the function to combine the accumulated value with each key-value pair
          * @return
          *   the final accumulated value after folding over all key-value pairs
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).foldLeft(0)(_ + _._2) === 3
          *   }}}
          */
        def foldLeft[C](init: C)(combiner: (C, (A, B)) => C): C =
            self.toList.foldLeft(init) { (acc, pair) => combiner(acc, pair) }

        /** Folds the `SortedMap` from the right, combining key-value pairs into a single value.
          *
          * @param init
          *   the initial value to start folding from
          * @param combiner
          *   the function to combine the accumulated value with each key-value pair
          * @return
          *   the final accumulated value after folding over all key-value pairs
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).foldLeft(0)(_ + _._2) === 3
          *   }}}
          */
        def foldRight[C](init: C)(combiner: ((A, B), C) => C): C =
            self.toList.foldRight(init) { (pair, acc) => combiner(pair, acc) }

    extension [A: Ord, B](self: SortedMap[A, B])
        /** Optionally returns the value associated with a key.
          *
          * @param key
          *   the key value
          * @return
          *   an option value containing the value associated with `key` in this map, or `None` if
          *   none exists.
          * @example
          *   {{{
          *   SortedMap.empty.get("key") === None
          *   SortedMap.singleton("key", "value").get("key") === Some("value")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).get("a") === Some(1)
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).get("c") === None
          *   }}}
          */
        def get(key: A): Option[B] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[B] = lst match
                case Nil => None
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            key <=> k match
                                case Order.Less    => None
                                case Order.Greater => go(tail)
                                case Order.Equal   => Some(v)

            go(self.toList)
        }

        /** Returns the value associated with a key, or fails with a message if the key is not
          * found.
          *
          * @param key
          *   the key value
          * @param message
          *   the error message to use if the key is not found
          * @return
          *   the value associated with `key` in this map
          * @throws NoSuchElementException
          *   if the key is not found in the map
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).getOrFail("a") === 1
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).getOrFail("c", "Key not found") // throws NoSuchElementException with message "Key not found"
          *   }}}
          */
        inline def getOrFail(key: A, inline message: String = "None.getOrFail"): B =
            get(key).getOrFail(message)

        /** Checks if the `SortedMap` contains a key.
          *
          * @param key
          *   the key to check for existence
          * @return
          *   `true` if the map contains the key, `false` otherwise
          * @example
          *   {{{
          *   SortedMap.empty.contains("key") === false
          *   SortedMap.singleton("key", "value").contains("key") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).contains("a") === true
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).contains("c") === false
          *   }}}
          */
        def contains(key: A): Boolean = get(key).isDefined

        /** Insert a key-value pair into the `SortedMap`, maintaining the sorted order without
          * duplication. * If the key already exists, it updates the value. * @param key the key to
          * insert
          * @param value
          *   the value associated with the key * @return a new `SortedMap` with the key-value pair
          *   inserted
          * @example
          *   {{{
          *   SortedMap.empty.insert("key", "value") === SortedMap.singleton("key", "value")
          *   SortedMap.singleton("key", "value").insert("key", "newValue") === SortedMap.singleton("key", "newValue")
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).insert("c", 3).toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Cons(("c", 3), List.Nil)))
          *   }}}
          */
        def insert(key: A, value: B): SortedMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil => single(key, value)
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            key <=> k match
                                case Order.Less    => Cons((key, value), lst)
                                case Order.Greater => Cons(pair, go(tail))
                                case Order.Equal   => Cons((key, value), tail)

            SortedMap(go(self.toList))
        }

        /** Deletes a key-value pair from the `SortedMap` by key.
          *
          * @param key
          *   the key to delete
          * @return
          *   a new `SortedMap` with the key-value pair removed, if it existed
          * @example
          *   {{{
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).delete("a").toList === List.Cons(("b", 2), List.Nil)
          *   SortedMap.fromList(List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))).delete("c").toList === List.Cons(("a", 1), List.Cons(("b", 2), List.Nil))
          *   }}}
          */
        def delete(key: A): SortedMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil => Nil
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            key <=> k match
                                case Order.Less    => lst
                                case Order.Greater => Cons(pair, go(tail))
                                case Order.Equal   => tail

            SortedMap(go(self.toList))
        }
}

case class Rational(numerator: BigInt, denominator: BigInt)

@Compile
object Rational:

    given Eq[Rational] = (lhs: Rational, rhs: Rational) =>
        lhs.numerator * rhs.denominator === rhs.numerator * lhs.denominator

    given rationalFromData: FromData[Rational] = FromData.derived

    given rationalToData: ToData[Rational] = ToData.derived
