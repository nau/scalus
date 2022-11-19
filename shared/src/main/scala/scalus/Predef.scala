package scalus

object Predef {
  enum List[+A]:
    case Nil extends List[Nothing]
    case Cons(head: A, tail: List[A])
    def map[B](f: A => B): List[B] = this match
      case Nil => Nil
      case Cons(head, tail) => Cons(f(head), tail.map(f))
  object List:
    def empty[A]: List[A] = Nil
    def apply[A](args: A*): List[A] = args.foldRight(empty[A])(Cons(_, _))
}
