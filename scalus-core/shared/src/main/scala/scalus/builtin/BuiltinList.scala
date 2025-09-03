package scalus.builtin

import scala.collection.immutable

enum BuiltinList[+A]:
    private case Nil extends BuiltinList[Nothing]

    private case Cons(h: A, tl: BuiltinList[A]) extends BuiltinList[A]

    def isEmpty: Boolean = this match
        case Nil => true
        case _   => false

    def head: A = this match
        case Cons(h, _) => h
        case _          => throw new NoSuchElementException("head of empty list")

    def tail: BuiltinList[A] = this match
        case Cons(_, t) => t
        case _          => throw new NoSuchElementException("tail of empty list")

    def ::[B >: A](x: B): BuiltinList[B] = Cons(x, this)

    def toList: immutable.List[A] = this match
        case Nil        => immutable.Nil
        case Cons(h, t) => h :: t.toList

object BuiltinList:
    def empty[A]: BuiltinList[A] = Nil
    def apply[A](xs: A*): BuiltinList[A] = xs.foldRight(empty[A])(_ :: _)
    def from[A](xs: IterableOnce[A]): BuiltinList[A] = xs.iterator.foldRight(empty[A])(_ :: _)

case class BuiltinPair[A, B](fst: A, snd: B):
    override def toString = "(" + fst + ", " + snd + ")"
