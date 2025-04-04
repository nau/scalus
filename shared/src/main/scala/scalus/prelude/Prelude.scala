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
    type Eq[A] = (A, A) => Boolean
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

        def getByIndex(idx: BigInt): A = {
            @tailrec
            def go(i: BigInt, lst: List[A]): A = lst match
                case Nil => throw new Exception("Index out of bounds")
                case Cons(head, tail) =>
                    if equalsInteger(i, idx) then head else go(addInteger(i, 1), tail)

            go(0, self)
        }

        @tailrec
        def contains(what: A)(using eq: Eq[A]): Boolean = self match
            case Nil              => false
            case Cons(head, tail) => if what === head then true else tail.contains(what)

        def groupBy[K](f: A => K): AssocMap[K, List[A]] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, List[A]]): AssocMap[K, List[A]] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val key = f(head)
                        AssocMap.lookup(acc)(key) match
                            case None =>
                                val newAcc = AssocMap.insert(acc)(key, List.single(head))
                                go(tail, newAcc)
                            case Some(value) =>
                                val newValue = value.prepended(head)
                                val newAcc = AssocMap.insert(acc)(key, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty[K, List[A]]).map { (k, v) => (k, v.reverse) }
        }

        def groupMap[K, B](key: A => K)(f: A => B): AssocMap[K, List[B]] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, List[B]]): AssocMap[K, List[B]] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val k = key(head)
                        val v = f(head)
                        AssocMap.lookup(acc)(k) match
                            case None =>
                                val newAcc = AssocMap.insert(acc)(k, List.single(v))
                                go(tail, newAcc)
                            case Some(value) =>
                                val newValue = value.prepended(v)
                                val newAcc = AssocMap.insert(acc)(k, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty[K, List[B]]).map { (k, v) => (k, v.reverse) }
        }

        def groupMapReduce[K, B](key: A => K)(f: A => B)(reduce: (B, B) => B): AssocMap[K, B] = {
            @tailrec
            def go(list: List[A], acc: AssocMap[K, B]): AssocMap[K, B] =
                list match
                    case Nil => acc
                    case Cons(head, tail) =>
                        val k = key(head)
                        val v = f(head)
                        AssocMap.lookup(acc)(k) match
                            case None =>
                                val newAcc = AssocMap.insert(acc)(k, v)
                                go(tail, newAcc)
                            case Some(value) =>
                                val newValue = reduce(value, v)
                                val newAcc = AssocMap.insert(acc)(k, newValue)
                                go(tail, newAcc)

            go(self, AssocMap.empty[K, B])
        }

        /** Adds an element at the beginning of this list */
        def prepended[B >: A](head: B): List[B] = Cons(head, self)

        def appendedAll[B >: A](other: List[B]): List[B] = self match
            case Nil              => other
            case Cons(head, tail) => Cons(head, tail.appendedAll(other))

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

        def length: BigInt = {
            @tailrec
            def count(lst: List[A], counter: BigInt): BigInt = lst match
                case Nil           => counter
                case Cons(_, tail) => count(tail, counter + 1)

            count(self, BigInt(0))
        }

        def size: BigInt = length

        def head: A = self match
            case Nil            => throw new NoSuchElementException("head of empty list")
            case Cons(value, _) => value

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
            if self.isEmpty then return immutable.List.empty[A]

            @tailrec
            def toListBuffer(
                list: List[A],
                listBuffer: mutable.ListBuffer[A]
            ): mutable.ListBuffer[A] =
                list match
                    case Nil              => listBuffer
                    case Cons(head, tail) => toListBuffer(tail, listBuffer.addOne(head))

            toListBuffer(self, mutable.ListBuffer.empty[A]).toList
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

        inline def get: A = self match
            case None        => throw new NoSuchElementException("None.get")
            case Some(value) => value

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

    extension [A](self: Option[Option[A]])
        def flatten: Option[A] = self match
            case None    => None
            case Some(a) => a

    extension [A](self: scala.Option[A])
        /** Converts a [[scala.Option]] to an `Option` */
        @Ignore
        def asScalus: Option[A] = o match
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

case class AssocMap[A, B](inner: List[(A, B)])

@Compile
object AssocMap {
    import List.*
    import Option.*
    def empty[A, B]: AssocMap[A, B] = AssocMap(List.empty[(A, B)])
    def singleton[A, B](key: A, value: B): AssocMap[A, B] = AssocMap(List.single((key, value)))
    def fromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = AssocMap(lst)
    def toList[A, B](map: AssocMap[A, B]): List[(A, B)] = map.inner

    def lookup[A: Eq, B](map: AssocMap[A, B])(key: A): Option[B] =
        @tailrec
        def go(lst: List[(A, B)]): Option[B] = lst match
            case Nil => Option.None
            case Cons(pair, tail) =>
                pair match
                    case (k, v) => if k === key then Option.Some(v) else go(tail)
        go(map.inner)

    def insert[A: Eq, B](map: AssocMap[A, B])(key: A, value: B): AssocMap[A, B] =
        def go(lst: List[(A, B)]): List[(A, B)] = lst match
            case Nil => List.Cons((key, value), List.Nil)
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        if k === key then List.Cons((key, value), tail)
                        else List.Cons(pair, go(tail))
        AssocMap(go(map.inner))

    def delete[A: Eq, B](map: AssocMap[A, B])(key: A): AssocMap[A, B] =
        def go(lst: List[(A, B)]): List[(A, B)] = lst match
            case Nil => List.Nil
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        if k === key then tail else List.Cons(pair, go(tail))
        AssocMap(go(map.inner))

    def union[A: Eq, B, C](
        lhs: AssocMap[A, B],
        rhs: AssocMap[A, C]
    ): AssocMap[A, These[B, C]] =
        def go(lst: List[(A, B)]): List[(A, These[B, C])] = lst match
            case Nil => List.Nil
            case Cons(pair, tail) =>
                pair match
                    case (k, v) =>
                        val optionR = AssocMap.lookup(rhs)(k)
                        val these = optionR match
                            case None    => These.This(v)
                            case Some(r) => These.These(v, r)
                        Cons((k, these), go(tail))

        val lhs1 = go(lhs.inner) // all left with corresponding right

        val rhsNotInLhs =
            rhs.inner.filter { case (a, c) => !lhs.inner.exists(p => p._1 === a) }

        val rhsThat = rhsNotInLhs.map { case (k, v) => (k, These.That(v)) }
        AssocMap(lhs1.appendedAll(rhsThat))

    def map[A, B, C](map: AssocMap[A, B])(f: ((A, B)) => (A, C)): AssocMap[A, C] =
        AssocMap(map.inner.map(f))

    def all[A, B](map: AssocMap[A, B])(f: ((A, B)) => Boolean): Boolean =
        map.inner.forall(f)
}

case class Rational(numerator: BigInt, denominator: BigInt)
