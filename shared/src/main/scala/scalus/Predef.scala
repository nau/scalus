package scalus

import scalus.builtins.ByteString
import scalus.builtins.Builtins

object Predef {
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
}
