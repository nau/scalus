package scalus.prelude

import scala.collection.immutable
import scalus.builtins.ByteString
import scalus.builtins.Builtins
import scalus.Compile

@Compile
object TestCode {
  def foo() = BigInt(42)
  val a = true
}

@Compile
object Prelude {
  type Eq[A] = (A, A) => Boolean
  given Eq[BigInt] = (x: BigInt, y: BigInt) => Builtins.equalsInteger(x, y)
  given Eq[ByteString] = (x: ByteString, y: ByteString) => Builtins.equalsByteString(x, y)
  given Eq[String] = (x: String, y: String) => Builtins.equalsString(x, y)

  extension [A](x: A) inline def ===(inline y: A)(using inline eq: Eq[A]): Boolean = eq(x, y)

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
  def empty[A]: List[A] = List.Nil

  def apply[A](args: A*): List[A] = args.foldRight(empty[A])(Cons(_, _))

  def append[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match
    case Nil              => lst2
    case Cons(head, tail) => Cons(head, append(tail, lst2))

  def map[A, B](lst: List[A])(f: A => B): List[B] = lst match
    case Nil              => List.Nil
    case Cons(head, tail) => Cons(f(head), List.map(tail)(f))

  def filter[A](lst: List[A])(p: A => Boolean): List[A] = lst match
    case Nil => List.Nil
    case Cons(head, tail) =>
      if p(head) then Cons(head, List.filter(tail)(p)) else List.filter(tail)(p)

  def findOrFail[A](lst: List[A])(p: A => Boolean): A = lst match
    case Nil              => throw new Exception("Not found")
    case Cons(head, tail) => if p(head) then head else findOrFail(tail)(p)

  def find[A](lst: List[A])(p: A => Boolean): Maybe[A] = lst match
    case Nil              => Maybe.Nothing
    case Cons(head, tail) => if p(head) then Maybe.Just(head) else find(tail)(p)

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

enum These[+A, +B]:
  case This(a: A)
  case That(b: B)
  case These(a: A, b: B)

opaque type AssocMap[A, B] = List[(A, B)]

@Compile
object AssocMap:
  import List.*
  import Maybe.*
  def empty[A, B]: AssocMap[A, B] = List.empty[(A, B)]
  def singleton[A, B](key: A, value: B): AssocMap[A, B] = List.Cons((key, value), List.Nil)
  def fromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = lst
  def toList[A, B](map: AssocMap[A, B]): List[(A, B)] = map
  def lookup[A: Eq, B](map: AssocMap[A, B])(key: A): Maybe[B] =
    def go(lst: List[(A, B)]): Maybe[B] = lst match
      case Nil => Maybe.Nothing
      case Cons(pair, tail) =>
        pair match
          case (k, v) => if k === key then Maybe.Just(v) else go(tail)
    go(map)

  def insert[A: Eq, B](map: AssocMap[A, B])(key: A, value: B): AssocMap[A, B] =
    def go(lst: List[(A, B)]): List[(A, B)] = lst match
      case Nil => List.Cons((key, value), List.Nil)
      case Cons(pair, tail) =>
        pair match
          case (k, v) =>
            if k === key then List.Cons((key, value), tail) else List.Cons(pair, go(tail))
    go(map)

  def delete[A: Eq, B](map: AssocMap[A, B])(key: A): AssocMap[A, B] =
    def go(lst: List[(A, B)]): List[(A, B)] = lst match
      case Nil => List.Nil
      case Cons(pair, tail) =>
        pair match
          case (k, v) =>
            if k === key then tail else List.Cons(pair, go(tail))
    go(map)

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
              case Nothing => These.This(v)
              case Just(r) => These.These(v, r)
            Cons((k, these), go(tail))

    val lhs1 = go(lhs) // all left with corresponding right

    val rhsNotInLhs =
      List.filter(rhs) { case (a, c) => !List.exists(lhs)(p => p._1 === a) }

    val rhsThat = List.map(rhsNotInLhs) { case (k, v) => (k, These.That(v)) }
    List.append(lhs1, rhsThat)
