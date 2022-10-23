package scalus.builtins
import scalus.uplc.Data

import scala.collection.immutable.Nil

case class Pair[A, B](fst: A, snd: B):
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

object List:
  def empty[A]: List[A] = Nil
  def apply[A](xs: A*): List[A] = xs.foldRight(empty[A])(_ :: _)

object Builtins:

  def unsafeDataAsConstr(d: Data): Pair[BigInt, List[Data]] = d match
    case Data.Constr(constr, args) => Pair(constr: BigInt, List(args: _*))
    case _                         => throw new Exception(s"not a constructor but $d")
  def unsafeDataAsList(d: Data): List[Data] = d match
    case Data.List(values) => List(values: _*)
    case _                 => throw new Exception(s"not a list but $d")

  def unsafeDataAsI(d: Data): BigInt = d match
    case Data.I(value) => value
    case _             => throw new Exception(s"not an integer but $d")

  def unsafeDataAsB(d: Data): Array[Byte] = d match
    case Data.B(value) => value
    case _             => throw new Exception(s"not a bytestring but $d")
