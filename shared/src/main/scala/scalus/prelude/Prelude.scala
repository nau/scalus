package scalus.prelude

import scalus.Compile
import scalus.Ignore
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.macros.Macros

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

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

@Compile
object Prelude {
    type Eq[-A] = (A, A) => Boolean
    // given Eq[Nothing] = (x: Nothing, y: Nothing) => throw new Exception("EQN")
    given Eq[BigInt] = (x: BigInt, y: BigInt) => equalsInteger(x, y)
    given Eq[ByteString] = (x: ByteString, y: ByteString) => equalsByteString(x, y)
    given Eq[String] = (x: String, y: String) => equalsString(x, y)
    given Eq[Boolean] = (x: Boolean, y: Boolean) => x == y
    given Eq[Data] = (x: Data, y: Data) => equalsData(x, y)
    given Eq[Unit] = (_: Unit, _: Unit) => true

    extension [A](x: A) inline def ===(inline y: A)(using inline eq: Eq[A]): Boolean = eq(x, y)
    extension [A](x: A) inline def !==(inline y: A)(using inline eq: Eq[A]): Boolean = !eq(x, y)

    def encodeHex(input: ByteString): String = {
        val len = lengthOfByteString(input)

        val byteToChar =
            (byte: BigInt) => if lessThanInteger(byte, 10) then byte + 48 else byte + 87

        def go(i: BigInt): ByteString = {
            if equalsInteger(i, len) then ByteString.fromHex("")
            else {
                val byte = indexByteString(input, i)
                val char1 = byteToChar(byte / 16)
                val char2 = byteToChar(byte % 16)
                consByteString(char1, consByteString(char2, go(i + 1)))
            }
        }
        decodeUtf8(go(0))
    }

    inline def log(msg: String): Unit = trace(msg)(())

    extension (b: Boolean)
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
    inline def empty[A]: List[A] = List.Nil

    /** Creates a list with a single element */
    def single[A](a: A): List[A] = Cons(a, List.Nil)

    @Ignore
    def apply[A](args: A*): List[A] = args.foldRight(empty[A]) { case (a, b) => Cons(a, b) }

    @Ignore
    def from[A](i: IterableOnce[A]): List[A] = i.iterator.foldRight(empty[A]) { case (a, b) =>
        Cons(a, b)
    }

    @Ignore
    def from[A](i: java.lang.Iterable[A]): List[A] = {
        import scala.jdk.CollectionConverters.*
        from(i.asScala)
    }

    def range(from: BigInt, to: BigInt): List[BigInt] = {
        require(lessThanEqualsInteger(from, to), "`from` must be less than or equal `to`")

        @tailrec
        def go(current: BigInt, acc: List[BigInt]): List[BigInt] =
            if lessThanInteger(current, to) then go(addInteger(current, 1), Cons(current, acc))
            else acc

        go(from, Nil).reverse
    }

    def fill[A](value: A, times: BigInt): List[A] = {
        require(greaterThanEqualsInteger(times, 0), "`times` must be greater than or equal 0")

        @tailrec
        def go(current: BigInt, acc: List[A]): List[A] =
            if lessThanInteger(current, times) then go(addInteger(current, 1), Cons(value, acc))
            else acc

        go(0, Nil).reverse
    }

    def map2[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = {
        a match
            case Cons(h1, t1) =>
                b match
                    case Cons(h2, t2) => Cons(f(h1, h2), map2(t1, t2)(f))
                    case Nil          => Nil
            case Nil => Nil
    }

    extension [A](self: List[A])
        inline def !!(idx: BigInt): A = self.getByIndex(idx)

        def isEmpty: Boolean = self match
            case Nil        => true
            case Cons(_, _) => false

        def nonEmpty: Boolean = self match
            case Nil        => false
            case Cons(_, _) => true

        def isDefinedAt(index: BigInt): Boolean =
            require(greaterThanEqualsInteger(index, 0), "`index` must be greater than or equal 0")

            @tailrec
            def go(lst: List[A], currentIndex: BigInt): Boolean = lst match
                case Nil => false
                case Cons(_, tail) =>
                    if equalsInteger(currentIndex, index) then true
                    else go(tail, addInteger(currentIndex, 1))

            go(self, 0)

        def getByIndex(index: BigInt): A = {
            require(greaterThanEqualsInteger(index, 0), "`index` must be greater than or equal 0")

            @tailrec
            def go(lst: List[A], currentIndex: BigInt): A = lst match
                case Nil => throw new Exception("Index out of bounds")
                case Cons(head, tail) =>
                    if equalsInteger(currentIndex, index) then head
                    else go(tail, addInteger(currentIndex, 1))

            go(self, 0)
        }

        def at(index: BigInt): Option[A] = {
            require(greaterThanEqualsInteger(index, 0), "`index` must be greater than or equal 0")

            @tailrec
            def go(lst: List[A], currentIndex: BigInt): Option[A] = lst match
                case Nil => None
                case Cons(head, tail) =>
                    if equalsInteger(currentIndex, index) then Some(head)
                    else go(tail, addInteger(currentIndex, 1))

            go(self, 0)
        }

        @tailrec
        def contains[B >: A](elem: B)(using eq: Eq[B]): Boolean = self match
            case Nil              => false
            case Cons(head, tail) => if elem === head then true else tail.contains(elem)

        def groupBy[K: Eq](f: A => K): AssocMap[K, List[A]] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, List[A]]): AssocMap[K, List[A]] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val key = f(head)
                        acc.lookup(key) match
                            case None =>
                                val newAcc = acc.insert(key, List.single(head))
                                go(tail, newAcc)
                            case Some(value) =>
                                val newValue = value.prepended(head)
                                val newAcc = acc.insert(key, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty).map { (k, v) => (k, v.reverse) }
        }

        def groupMap[K: Eq, B](key: A => K)(f: A => B): AssocMap[K, List[B]] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, List[B]]): AssocMap[K, List[B]] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val k = key(head)
                        val v = f(head)
                        acc.lookup(k) match
                            case None =>
                                val newAcc = acc.insert(k, List.single(v))
                                go(tail, newAcc)
                            case Some(value) =>
                                val newValue = value.prepended(v)
                                val newAcc = acc.insert(k, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty).map { (k, v) => (k, v.reverse) }
        }

        def groupMapReduce[K: Eq, B](
            key: A => K
        )(f: A => B)(reduce: (B, B) => B): AssocMap[K, B] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, B]): AssocMap[K, B] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val k = key(head)
                        val v = f(head)
                        acc.lookup(k) match
                            case None =>
                                val newAcc = acc.insert(k, v)
                                go(tail, newAcc)
                            case Some(value) =>
                                val newValue = reduce(value, v)
                                val newAcc = acc.insert(k, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty)
        }

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

        def map[B](f: A => B): List[B] = self match
            case Nil              => Nil
            case Cons(head, tail) => Cons(f(head), tail.map(f))

        def filter(p: A => Boolean): List[A] = self match
            case Nil => Nil
            case Cons(head, tail) =>
                if p(head) then Cons(head, tail.filter(p)) else tail.filter(p)

        @tailrec
        def find(p: A => Boolean): Option[A] = self match
            case Nil              => None
            case Cons(head, tail) => if p(head) then Some(head) else tail.find(p)

        @tailrec
        def foldLeft[B](z: B)(f: (B, A) => B): B = self match
            case Nil              => z
            case Cons(head, tail) => tail.foldLeft(f(z, head))(f)

        @tailrec
        def exists(p: A => Boolean): Boolean = self match
            case Nil              => false
            case Cons(head, tail) => if p(head) then true else tail.exists(p)

        @tailrec
        def forall(p: A => Boolean): Boolean = self match
            case Nil              => true
            case Cons(head, tail) => if p(head) then tail.forall(p) else false

        def count(p: A => Boolean): BigInt = {
            @tailrec
            def go(lst: List[A], counter: BigInt): BigInt = lst match
                case Nil => counter
                case Cons(head, tail) =>
                    if p(head) then go(tail, addInteger(counter, 1)) else go(tail, counter)

            go(self, BigInt(0))
        }

        def indexOf[B >: A](elem: B)(using eq: Eq[B]): BigInt = {
            @tailrec
            def go(lst: List[A], index: BigInt): BigInt = lst match
                case Nil => -1
                case Cons(head, tail) =>
                    if head === elem then index else go(tail, addInteger(index, 1))

            go(self, BigInt(0))
        }

        def indexOfOption[B >: A](elem: B)(using eq: Eq[B]): Option[BigInt] = {
            @tailrec
            def go(lst: List[A], index: BigInt): Option[BigInt] = lst match
                case Nil => None
                case Cons(head, tail) =>
                    if head === elem then Some(index) else go(tail, addInteger(index, 1))

            go(self, BigInt(0))
        }

        @tailrec
        def last: A = self match
            case Nil               => throw new NoSuchElementException("last of empty list")
            case Cons(value, tail) => if tail.isEmpty then value else tail.last

        @tailrec
        def lastOption: Option[A] = self match
            case Nil               => None
            case Cons(value, tail) => if tail.isEmpty then Some(value) else tail.lastOption

        def length: BigInt = {
            @tailrec
            def go(lst: List[A], counter: BigInt): BigInt = lst match
                case Nil           => counter
                case Cons(_, tail) => go(tail, addInteger(counter, 1))

            go(self, BigInt(0))
        }

        inline def size: BigInt = length

        def head: A = self match
            case Nil            => throw new NoSuchElementException("head of empty list")
            case Cons(value, _) => value

        def headOption: Option[A] = self match
            case Nil            => None
            case Cons(value, _) => Some(value)

        def tail: List[A] = self match
            case Nil           => throw new NoSuchElementException("tail of empty list")
            case Cons(_, rest) => rest

        def reverse: List[A] = {
            @tailrec
            def go(list: List[A], acc: List[A]): List[A] = list match
                case Nil              => acc
                case Cons(head, tail) => go(tail, Cons(head, acc))

            go(self, Nil)
        }

        @tailrec
        def foreach(f: A => Unit): Unit = self match
            case Nil              => ()
            case Cons(head, tail) => f(head); tail.foreach(f)

        /** Converts a `List` to a [[scala.List]] */
        @Ignore
        def asScala: immutable.List[A] = {
            if self.isEmpty then return immutable.List.empty

            @tailrec
            def toListBuffer(
                list: List[A],
                listBuffer: mutable.ListBuffer[A]
            ): mutable.ListBuffer[A] =
                list match
                    case Nil              => listBuffer
                    case Cons(head, tail) => toListBuffer(tail, listBuffer.addOne(head))

            toListBuffer(self, mutable.ListBuffer.empty).toList
        }

enum Option[+A]:
    case None extends Option[Nothing]
    case Some(value: A)

@Compile
object Option {

    /** Constructs a `Option` from a value. If the value is `null`, it returns `None`, otherwise
      * `Some(value)`.
      */
    @Ignore
    inline def apply[A](x: A): Option[A] = if x == null then None else Some(x)

    inline def empty[A]: Option[A] = None

    extension [A](self: Option[A])
        def isEmpty: Boolean = self match
            case None    => true
            case Some(_) => false

        def nonEmpty: Boolean = self match
            case None    => false
            case Some(_) => true

        def isDefined: Boolean = nonEmpty

        inline def getOrFail(inline message: String = "None.getOrFail"): A = self match
            case None        => throw new NoSuchElementException(message)
            case Some(value) => value

        inline infix def orFail(inline message: String = "None.orFail"): Unit = self match
            case None    => throw new NoSuchElementException(message)
            case Some(_) => ()

        def get: A = self match
            case None        => throw new NoSuchElementException("None.get")
            case Some(value) => value

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

        def map[B](f: A => B): Option[B] = self match
            case None    => None
            case Some(a) => Some(f(a))

        def flatMap[B](f: A => Option[B]): Option[B] = self match
            case None    => None
            case Some(a) => f(a)

        def filter(p: A => Boolean): Option[A] = self match
            case None    => None
            case Some(a) => if p(a) then self else None

        def filterNot(p: A => Boolean): Option[A] = self match
            case None    => None
            case Some(a) => if p(a) then None else self

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
        def lookup(key: A): Option[B] = {
            @tailrec
            def go(lst: List[(A, B)]): Option[B] = lst match
                case Nil => Option.None
                case Cons(pair, tail) =>
                    pair match
                        case (k, v) => if k === key then Option.Some(v) else go(tail)

            go(self.toList)
        }

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
                        val optionR = rhs.lookup(k)
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
}

case class Rational(numerator: BigInt, denominator: BigInt)
