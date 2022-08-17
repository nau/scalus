package scalus.uplc

import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable
import scala.deriving.*
import scala.quoted.*

sealed abstract class Data
object Data:
  trait Lift[A]:
    def lift(a: A): Data

  object Lift:
    import scala.compiletime.*
    inline def summonAll[T <: Tuple]: immutable.List[Lift[_]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts)  => summonInline[Lift[t]] :: summonAll[ts]
    inline def derived[T](using m: Mirror.Of[T]): Lift[T] =
      val elemInstances = summonAll[m.MirroredElemTypes]
//      println(elemInstances)
      m match
        case m: Mirror.ProductOf[T] =>
          new Lift[T] {
            def lift(a: T): Data =
              Data.Constr(
                0,
                elemInstances
                  .zip(a.asInstanceOf[Product].productIterator)
                  .map { case (l, a) =>
                    l.asInstanceOf[Lift[Any]].lift(a)
                  }
              )
          }

  extension [A: Lift](a: A) def toData: Data = summon[Lift[A]].lift(a)

  given Lift[BigInt] with { def lift(a: BigInt): Data = I(a) }
  given Lift[Int] with { def lift(a: Int): Data = I(a) }
  given Lift[Array[Byte]] with { def lift(a: Array[Byte]): Data = B(a) }
  given seqLift[A: Lift, B[A] <: Seq[A]]: Lift[B[A]] with {
    def lift(a: B[A]): Data = List(a.map(summon[Lift[A]].lift).toList)
  }

  given mapLift[A: Lift, B: Lift]: Lift[immutable.Map[A, B]] with {
    def lift(a: immutable.Map[A, B]): Data = Map(a.toList.map { case (a, b) =>
      (summon[Lift[A]].lift(a), summon[Lift[B]].lift(b))
    })
  }

  case class Constr(constr: Long, args: immutable.List[Data]) extends Data

  case class Map(values: immutable.List[(Data, Data)]) extends Data

  case class List(values: immutable.List[Data]) extends Data:
    override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"

  case class I(value: BigInt) extends Data

  case class B(value: Array[Byte]) extends Data:

    override def toString: String = s"B(\"${bytesToHex(value)}\")"

    override def equals(that: Any): Boolean = that match
      case that: B =>
        that.canEqual(this) &&
        util.Arrays.equals(value, that.value)
      case _ => false

    // Step 8 - implement a corresponding hashCode c=method
    override def hashCode: Int = util.Arrays.hashCode(value)
