package scalus.builtin

import scala.collection.immutable

enum List[+A]:
    private case Nil extends List[Nothing]

    private case Cons(h: A, tl: List[A]) extends List[A]

    def isEmpty: Boolean = this match
        case Nil => true
        case _   => false

    def head: A = this match
        case Cons(h, _) => h
        case _          => throw new NoSuchElementException("head of empty list")

    def tail: List[A] = this match
        case Cons(_, t) => t
        case _          => throw new NoSuchElementException("tail of empty list")

    def ::[B >: A](x: B): List[B] = Cons(x, this)

    def toList: immutable.List[A] = this match
        case Nil        => immutable.Nil
        case Cons(h, t) => h :: t.toList

object List:
    def empty[A]: List[A] = Nil
    def apply[A](xs: A*): List[A] = xs.foldRight(empty[A])(_ :: _)

case class Pair[A, B](fst: A, snd: B):
    override def toString = "(" + fst + ", " + snd + ")"
