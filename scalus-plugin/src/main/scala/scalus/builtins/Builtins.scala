package scalus.builtins
import scalus.uplc.Data

import scala.collection.immutable

class ByteString private (val bytes: Array[Byte]) {
  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
    case _                => false
  }

}

object ByteString {
  val empty = new ByteString(Array.empty)
  def apply(bytes: Array[Byte]): ByteString = new ByteString(bytes.toArray)

  def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

  def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)
}

case class Pair[+A, +B](fst: A, snd: B):
  override def toString = "(" + fst + ", " + snd + ")"

enum List[+A]:
  case Nil extends List[Nothing]

  case Cons(h: A, tl: List[A]) extends List[A]

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


