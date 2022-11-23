package scalus

import scalus.builtins.ByteString

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
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), tail.map(f))
  object List:
    def empty[A]: List[A] = Nil
    def apply[A](args: A*): List[A] = args.foldRight(empty[A])(Cons(_, _))

  enum Maybe[+A]:
    case Nothing extends Maybe[Nothing]
    case Just(value: A)
}
