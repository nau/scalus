package scalus

import scalus.builtins.ByteString
import scalus.builtins.Builtins

object Prelude {
  sealed class Eq[A]
  given Eq[BigInt] = new Eq[BigInt]
  given Eq[ByteString] = new Eq[ByteString]
  given Eq[String] = new Eq[String]
  extension [A](x: A) def ===(y: A)(using Eq[A]): Boolean = x == y

  enum List[+A]:
    case Nil extends List[Nothing]
    case Cons(head: A, tail: List[A])
    def map[B](f: A => B): List[B] = this match
      case Nil              => Nil
      case Cons(head, tail) => Cons(f(head), tail.map(f))
  object List:
    def empty[A]: List[A] = Nil
    def apply[A](args: A*): List[A] = args.foldRight(empty[A])(Cons(_, _))
    def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
      case Nil              => throw new Exception("Not found")
      case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)
    def find[A](lst: List[A])(p: A => Boolean): Maybe[A] = lst match
      case Nil              => Maybe.Nothing
      case Cons(head, tail) => if p(head) then Maybe.Just(head) else find(tail)(p)
    def exists[A](lst: List[A])(p: A => Boolean): Boolean = lst match
      case Nil              => false
      case Cons(head, tail) => p(head) || exists(tail)(p)
    def foldLeft[A, B](lst: List[A], z: B)(f: (B, A) => B): B = lst match
      case Nil              => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)

  enum Maybe[+A]:
    case Nothing extends Maybe[Nothing]
    case Just(value: A)

  def encodeHex(input: ByteString): String = {
    val len = Builtins.lengthOfByteString(input)

    val byteToChar =
      (byte: BigInt) => if Builtins.lessThanInteger(byte, 10) then byte + 48 else byte + 87

    def go(i: BigInt): ByteString = {
      if i === len then ByteString.fromHex("")
      else {
        val byte = Builtins.indexByteString(input, i)
        val char1 = byteToChar(byte / 16)
        val char2 = byteToChar(byte % 16)
        Builtins.consByteString(char1, Builtins.consByteString(char2, go(i + 1)))
      }
    }
    Builtins.decodeUtf8(go(0))
  }

  opaque type AssocMap[A, B] = List[(A, B)]
  object AssocMap:
    def empty[A, B]: AssocMap[A, B] = List.empty[(A, B)]
    def fromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = lst
    def toList[A, B](map: AssocMap[A, B]): List[(A, B)] = map
    def lookup[A: Eq, B](key: A)(map: AssocMap[A, B]): Maybe[B] =
      def go(lst: List[(A, B)]): Maybe[B] = lst match
        case List.Nil => Maybe.Nothing
        case List.Cons(pair, tail) =>
          pair match
            case (k, v) => if k === key then Maybe.Just(v) else go(tail)
      go(map)
    def insert[A: Eq, B](key: A, value: B)(map: AssocMap[A, B]): AssocMap[A, B] =
      def go(lst: List[(A, B)]): List[(A, B)] = lst match
        case List.Nil => List.Cons((key, value), List.Nil)
        case List.Cons(pair, tail) =>
          pair match
            case (k, v) =>
              if k === key then List.Cons((key, value), tail) else List.Cons(pair, go(tail))
      go(map)
    def delete[A: Eq, B](key: A)(map: AssocMap[A, B]): AssocMap[A, B] =
      def go(lst: List[(A, B)]): List[(A, B)] = lst match
        case List.Nil => List.Nil
        case List.Cons(pair, tail) =>
          pair match
            case (k, v) =>
              if k === key then tail else List.Cons(pair, go(tail))
      go(map)
}
