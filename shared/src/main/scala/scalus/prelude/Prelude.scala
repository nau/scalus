package scalus.prelude

import scalus.Compile
import scalus.Ignore
import scalus.builtin.Builtins.*
import scalus.builtin.Data.{fromData, toData}
import scalus.builtin.{ByteString, Data, FromData, ToData}
import scalus.macros.Macros

import scala.annotation.{nowarn, tailrec}
import scala.collection.mutable

import Ord.*

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
        val len = lengthOfByteString(input)

        val byteToChar =
            (byte: BigInt) => if byte < 10 then byte + 48 else byte + 87

        def go(i: BigInt): ByteString = {
            if i == len then ByteString.fromHex("")
            else {
                val byte = indexByteString(input, i)
                val char1 = byteToChar(byte / 16)
                val char2 = byteToChar(byte % 16)
                consByteString(char1, consByteString(char2, go(i + 1)))
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

import Prelude.*

/** Tests an expression, throwing an `IllegalArgumentException` if false.
  * @param requirement
  *   the expression to test
  * @note
  *   we do not use scala.Predef.require because it's not an `inline` method and it's not expanded
  *   before Scalus compiler plugin phase.
  * @example
  *   {{{
  *   require(value > 1000, "Not enough")
  *   }}}
  */
inline def require(inline requirement: Boolean, inline message: String): Unit =
    if !requirement then throw new IllegalArgumentException(message)

inline def require(inline requirement: Boolean): Unit =
    if !requirement then throw new IllegalArgumentException()

inline def fail(inline message: String): Nothing = throw new RuntimeException(message)

inline def fail(): Nothing = throw new RuntimeException()

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

        def contains[B >: A](elem: B)(using eq: Eq[B]): Boolean = find(_ === elem).isDefined

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

            go(self, AssocMap.empty).map { (key, list) => (key, list.reverse) }
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
        inline def +:[B >: A](elem: B): List[B] = prepended(elem)
        inline def prependedAll[B >: A](other: List[B]): List[B] = other.appendedAll(self)
        inline def ++:[B >: A](other: List[B]): List[B] = prependedAll(other)

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
        def drop(skip: BigInt): List[A] = self match
            case Nil => Nil
            case Cons(_, tail) =>
                if skip <= 0 then self
                else tail.drop(skip - 1)

        def dropRight(skip: BigInt): List[A] =
            if skip <= 0 then self
            else
                self.foldRight((List.empty[A], skip)) { (head, acc) =>
                    if acc._2 > 0 then (Nil, acc._2 - 1)
                    else (Cons(head, acc._1), acc._2)
                }._1

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

enum Option[+A]:
    case None extends Option[Nothing]
    case Some(value: A)

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
    def fromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = AssocMap(lst)

    given AssocMapFromData[A: FromData, B: FromData]: FromData[AssocMap[A, B]] =
        (d: Data) =>
            def loop(
                ls: scalus.builtin.List[scalus.builtin.Pair[Data, Data]]
            ): scalus.prelude.List[(A, B)] =
                if ls.isEmpty then List.Nil
                else
                    val pair = ls.head
                    new List.Cons(
                      (fromData[A](pair.fst), fromData[B](pair.snd)),
                      loop(ls.tail)
                    )
            AssocMap.fromList(loop(unMapData(d)))

    given assocMapToData[A: ToData, B: ToData]: ToData[AssocMap[A, B]] =
        (a: AssocMap[A, B]) => {
            def go(a: List[(A, B)]): scalus.builtin.List[scalus.builtin.Pair[Data, Data]] =
                a match {
                    case List.Nil => mkNilPairData()
                    case List.Cons(tuple, tail) =>
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
        def map[C](f: ((A, B)) => (A, C)): AssocMap[A, C] = AssocMap(self.toList.map(f))
        def all(f: ((A, B)) => Boolean): Boolean = self.toList.forall(f)

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
                case Nil => Option.None
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) => if k === key then Option.Some(v) else go(tail)

            go(self.toList)
        }

        @deprecated("Use `get` instead")
        def lookup(key: A): Option[B] = get(key)

        def insert(key: A, value: B): AssocMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil => List.Cons((key, value), List.Nil)
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            if k === key then List.Cons((key, value), tail)
                            else List.Cons(pair, go(tail))

            AssocMap(go(self.toList))
        }

        def delete(key: A): AssocMap[A, B] = {
            def go(lst: List[(A, B)]): List[(A, B)] = lst match
                case Nil => List.Nil
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) =>
                            if k === key then tail else List.Cons(pair, go(tail))

            AssocMap(go(self.toList))
        }

    def union[A: Eq, B, C](
        lhs: AssocMap[A, B],
        rhs: AssocMap[A, C]
    ): AssocMap[A, These[B, C]] = {
        def go(lst: List[(A, B)]): List[(A, These[B, C])] = lst match
            case Nil => List.Nil
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

case class Rational(numerator: BigInt, denominator: BigInt)

@Compile
object Rational:

    given Eq[Rational] = (lhs: Rational, rhs: Rational) =>
        lhs.numerator * rhs.denominator === rhs.numerator * lhs.denominator

    given rationalFromData: FromData[Rational] = FromData.derived

    given rationalToData: ToData[Rational] = ToData.derived
