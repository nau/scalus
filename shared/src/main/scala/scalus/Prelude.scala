package scalus

import scala.collection.immutable
import scalus.builtins.ByteString
import scalus.builtins.Builtins

object Prelude {
  type Eq[A] = (A, A) => Boolean
  given Eq[BigInt] = (x: BigInt, y: BigInt) => Builtins.equalsInteger(x, y)
  given Eq[ByteString] = (x: ByteString, y: ByteString) => Builtins.equalsByteString(x, y)
  given Eq[String] = (x: String, y: String) => Builtins.equalsString(x, y)
  extension [A](x: A) def ===(y: A)(using eq: Eq[A]): Boolean = eq(x, y)

  enum List[+A]:
    case Nil extends List[Nothing]
    case Cons(head: A, tail: List[A])
    def toList: immutable.List[A] = this match
      case Nil              => immutable.List.empty[A]
      case Cons(head, tail) => head :: tail.toList
  object List:
    import Maybe.*
    def empty[A]: List[A] = List.Nil

    def apply[A](args: A*): List[A] = args.foldRight(empty[A])(Cons(_, _))

    def append[A](lst1: List[A], lst2: List[A]): List[A] = lst1 match
      case Nil              => lst2
      case Cons(head, tail) => Cons(head, append(tail, lst2))

    def map[A, B](f: A => B)(lst: List[A]): List[B] = lst match
      case Nil              => List.Nil
      case Cons(head, tail) => Cons(f(head), List.map(f)(tail))

    def filter[A](p: A => Boolean)(lst: List[A]): List[A] = lst match
      case Nil => List.Nil
      case Cons(head, tail) =>
        if p(head) then Cons(head, List.filter(p)(tail)) else List.filter(p)(tail)

    def findOrFail[A](lst: List[A])(p: A => Boolean): Unit = lst match
      case Nil              => throw new Exception("Not found")
      case Cons(head, tail) => if p(head) then () else findOrFail(tail)(p)

    def find[A](lst: List[A])(p: A => Boolean): Maybe[A] = lst match
      case Nil              => Maybe.Nothing
      case Cons(head, tail) => if p(head) then Maybe.Just(head) else find(tail)(p)

    def exists[A](lst: List[A])(p: A => Boolean): Boolean = find(lst)(p) match
      case Nothing => false
      case Just(a) => true

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

  enum These[+A, +B]:
    case This(a: A)
    case That(b: B)
    case These(a: A, b: B)

  opaque type AssocMap[A, B] = List[(A, B)]
  object AssocMap:
    import List.*
    import Maybe.*
    import These.*
    def empty[A, B]: AssocMap[A, B] = List.empty[(A, B)]
    def singleton[A, B](key: A, value: B): AssocMap[A, B] = List.Cons((key, value), List.Nil)
    def fromList[A, B](lst: List[(A, B)]): AssocMap[A, B] = lst
    def toList[A, B](map: AssocMap[A, B]): List[(A, B)] = map
    def lookup[A: Eq, B](key: A)(map: AssocMap[A, B]): Maybe[B] =
      def go(lst: List[(A, B)]): Maybe[B] = lst match
        case Nil => Maybe.Nothing
        case Cons(pair, tail) =>
          pair match
            case (k, v) => if k === key then Maybe.Just(v) else go(tail)
      go(map)

    def insert[A: Eq, B](key: A, value: B)(map: AssocMap[A, B]): AssocMap[A, B] =
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
    ): AssocMap[A, Prelude.These[B, C]] =
      def go(lst: List[(A, B)]): List[(A, Prelude.These[B, C])] = lst match
        case Nil => List.Nil
        case Cons(pair, tail) =>
          pair match
            case (k, v) =>
              val maybeR = AssocMap.lookup(k)(rhs)
              val these = maybeR match
                case Nothing => This(v)
                case Just(r) => Prelude.These.These(v, r)
              Cons((k, these), go(tail))

      val lhs1 = go(lhs) // all left with corresponding right

      val rhsNotInLhs =
        List.filter((pair: (A, C)) => !List.exists(lhs)(p => p._1 === pair._1))(rhs)

      val rhsThat =
        List.map[(A, C), (A, Prelude.These[B, C])]((pair: (A, C)) => (pair._1, That(pair._2)))(
          rhsNotInLhs
        )
      List.append(lhs1, rhsThat)
}