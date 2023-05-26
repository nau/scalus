package scalus.prelude

import scala.collection.immutable
import scalus.builtins.ByteString
import scalus.builtins.Builtins
import scalus.Compile
import scalus.Ignore
import scalus.uplc.Data

@Compile
object Prelude {
  type Eq[A] = (A, A) => Boolean
  // given Eq[Nothing] = (x: Nothing, y: Nothing) => throw new Exception("EQN")
  given Eq[BigInt] = (x: BigInt, y: BigInt) => Builtins.equalsInteger(x, y)
  given Eq[ByteString] = (x: ByteString, y: ByteString) => Builtins.equalsByteString(x, y)
  given Eq[String] = (x: String, y: String) => Builtins.equalsString(x, y)
  given Eq[Boolean] = (x: Boolean, y: Boolean) =>
    if x then if y then true else false else if y then false else true
  given Eq[Data] = (x: Data, y: Data) => Builtins.equalsData(x, y)
  given Eq[Unit] = (x: Unit, y: Unit) => true

  extension [A](x: A) inline def ===(inline y: A)(using inline eq: Eq[A]): Boolean = eq(x, y)
  extension [A](x: A) inline def !==(inline y: A)(using inline eq: Eq[A]): Boolean = !eq(x, y)

  def encodeHex(input: ByteString): String = {
    val len = Builtins.lengthOfByteString(input)

    val byteToChar =
      (byte: BigInt) => if Builtins.lessThanInteger(byte, 10) then byte + 48 else byte + 87

    def go(i: BigInt): ByteString = {
      if Builtins.equalsInteger(i, len) then ByteString.fromHex("")
      else {
        val byte = Builtins.indexByteString(input, i)
        val char1 = byteToChar(byte / 16)
        val char2 = byteToChar(byte % 16)
        Builtins.consByteString(char1, Builtins.consByteString(char2, go(i + 1)))
      }
    }
    Builtins.decodeUtf8(go(0))
  }

}

import Prelude.{*, given}

enum List[+A]:
  case Nil extends List[Nothing]
  case Cons(head: A, tail: List[A])
  def toList: immutable.List[A] = this match
    case Nil              => immutable.List.empty[A]
    case Cons(head, tail) => head :: tail.toList

@Compile
object List:
  import Maybe.*
  inline def empty[A]: List[A] = List.Nil

  extension [A](lst: List[A]) inline def !!(idx: BigInt): A = getByIndex(lst)(idx)

  @Ignore
  def apply[A](args: A*): List[A] = args.foldRight(empty[A]) { case (a, b) => new Cons(a, b) }

  def isEmpty[A](lst: List[A]): Boolean = lst match
    case Nil        => true
    case Cons(_, _) => false

  def getByIndex[A](lst: List[A])(idx: BigInt): A = {
    def go(i: BigInt, lst: List[A]): A = lst match
      case Nil => throw new Exception("Index out of bounds")
      case Cons(head, tail) =>
        if Builtins.equalsInteger(i, idx) then head else go(Builtins.addInteger(i, 1), tail)
    go(0, lst)
  }

  def append[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match
    case Nil              => lst2
    case Cons(head, tail) => new Cons(head, append(tail, lst2))

  def map[A, B](lst: List[A])(f: A => B): List[B] = lst match
    case Nil              => List.Nil
    case Cons(head, tail) => new Cons(f(head), List.map(tail)(f))

  def filter[A](lst: List[A])(p: A => Boolean): List[A] = lst match
    case Nil => List.Nil
    case Cons(head, tail) =>
      if p(head) then new Cons(head, List.filter(tail)(p)) else List.filter(tail)(p)

  def findOrFail[A](lst: List[A])(p: A => Boolean): A = lst match
    case Nil              => throw new Exception("Not found")
    case Cons(head, tail) => if p(head) then head else findOrFail(tail)(p)

  def find[A](lst: List[A])(p: A => Boolean): Maybe[A] = lst match
    case Nil              => Maybe.Nothing
    case Cons(head, tail) => if p(head) then new Maybe.Just(head) else find(tail)(p)

  def exists[A](lst: List[A])(p: A => Boolean): Boolean = find(lst)(p) match
    case Nothing => false
    case Just(a) => true

  def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match
    case Nil              => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)

  def all[A, B](lst: List[A])(f: A => Boolean): Boolean =
    foldLeft(lst, true)((acc, x) => acc && f(x))

enum Maybe[+A]:
  case Nothing extends Maybe[Nothing]
  case Just(value: A)

@Compile
object Maybe {
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

case class AssocMap[A, B](inner: List[(A, B)])

@Compile
object AssocMap {
  import List.*
  import Maybe.*
  def empty[A, B]: AssocMap[A, B] = new AssocMap(List.empty[(A, B)])
  def singleton[A, B](key: A, value: B): AssocMap[A, B] = new AssocMap(
    new List.Cons((key, value), List.Nil)
  )
  def fromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = new AssocMap(lst)
  def toList[A, B](map: AssocMap[A, B]): List[(A, B)] = map.inner
  def lookup[A: Eq, B](map: AssocMap[A, B])(key: A): Maybe[B] =
    def go(lst: List[(A, B)]): Maybe[B] = lst match
      case Nil => Maybe.Nothing
      case Cons(pair, tail) =>
        pair match
          case (k, v) => if k === key then new Maybe.Just(v) else go(tail)
    go(map.inner)

  def insert[A: Eq, B](map: AssocMap[A, B])(key: A, value: B): AssocMap[A, B] =
    def go(lst: List[(A, B)]): List[(A, B)] = lst match
      case Nil => new List.Cons((key, value), List.Nil)
      case Cons(pair, tail) =>
        pair match
          case (k, v) =>
            if k === key then new List.Cons((key, value), tail) else new List.Cons(pair, go(tail))
    new AssocMap(go(map.inner))

  def delete[A: Eq, B](map: AssocMap[A, B])(key: A): AssocMap[A, B] =
    def go(lst: List[(A, B)]): List[(A, B)] = lst match
      case Nil => List.Nil
      case Cons(pair, tail) =>
        pair match
          case (k, v) =>
            if k === key then tail else new List.Cons(pair, go(tail))
    new AssocMap(go(map.inner))

  def union[A: Eq, B, C](
      lhs: AssocMap[A, B],
      rhs: AssocMap[A, C]
  ): AssocMap[A, These[B, C]] =
    def go(lst: List[(A, B)]): List[(A, These[B, C])] = lst match
      case Nil => List.Nil
      case Cons(pair, tail) =>
        pair match
          case (k, v) =>
            val maybeR = AssocMap.lookup(rhs)(k)
            val these = maybeR match
              case Nothing => new These.This(v)
              case Just(r) => new These.These(v, r)
            new Cons((k, these), go(tail))

    val lhs1 = go(lhs.inner) // all left with corresponding right

    val rhsNotInLhs =
      List.filter(rhs.inner) { case (a, c) => !List.exists(lhs.inner)(p => p._1 === a) }

    val rhsThat = List.map(rhsNotInLhs) { case (k, v) => (k, new These.That(v)) }
    new AssocMap(List.append(lhs1, rhsThat))

  def map[A, B, C](map: AssocMap[A, B])(f: ((A, B)) => (A, C)): AssocMap[A, C] =
    new AssocMap(List.map(map.inner)(f))
}
