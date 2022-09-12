package scalus.uplc

import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable
import scala.collection.immutable.List
import scala.deriving.*
import scala.quoted.*

sealed abstract class Data
object Data:
  trait ToData[A]:
    def toData(a: A): Data

  object ToData:
    import scala.compiletime.*
    inline def summonAll[T <: Tuple]: immutable.List[ToData[_]] =
      inline erasedValue[T] match
        case _: EmptyTuple => Nil
        case _: (t *: ts)  => summonInline[ToData[t]] :: summonAll[ts]

    def liftSum[T](m: Mirror.SumOf[T], elems: => immutable.List[ToData[_]]): ToData[T] =
      new ToData[T] {
        def toData(a: T): Data =
          Data.Constr(
            m.ordinal(a),
            elems
              .zip(a.asInstanceOf[Product].productIterator)
              .map { case (l, a) =>
                l.asInstanceOf[ToData[Any]].toData(a)
              }
          )
      }
    def liftProduct[T](
        constrIdx: Int,
        p: Mirror.ProductOf[T],
        elems: => immutable.List[ToData[_]]
    ): ToData[T] =
      new ToData[T] {
        def toData(a: T): Data =
          Data.Constr(
            constrIdx,
            elems.zip(a.asInstanceOf[Product].productIterator).map { case (l, a) =>
              l.asInstanceOf[ToData[Any]].toData(a)
            }
          )
      }

    inline def derived[T](using m: Mirror.Of[T]): ToData[T] =
      val elemInstances = summonAll[m.MirroredElemTypes]
//      println(elemInstances)
      inline m match
        case m: Mirror.SumOf[T]     => liftSum(m, elemInstances)
        case m: Mirror.ProductOf[T] => liftProduct(0, m, elemInstances)

    inline def deriveProduct[T](constrIdx: Int)(using m: Mirror.ProductOf[T]): ToData[T] =
      val elemInstances = summonAll[m.MirroredElemTypes]
      liftProduct(constrIdx, m, elemInstances)

  extension [A: ToData](a: A) def toData: Data = summon[ToData[A]].toData(a)

  given ToData[Boolean] with {
    def toData(a: Boolean): Data = if a then Constr(1, Nil) else Constr(0, Nil)
  }
  given ToData[Data] with { def toData(a: Data): Data = a }
  given ToData[BigInt] with { def toData(a: BigInt): Data = I(a) }
  given ToData[Int] with { def toData(a: Int): Data = I(a) }
  given ToData[Array[Byte]] with { def toData(a: Array[Byte]): Data = B(a) }
  given seqToData[A: ToData, B[A] <: Seq[A]]: ToData[B[A]] with {
    def toData(a: B[A]): Data = List(a.map(summon[ToData[A]].toData).toList)
  }

  given mapToData[A: ToData, B: ToData]: ToData[immutable.Map[A, B]] with {
    def toData(a: immutable.Map[A, B]): Data = Map(a.toList.map { case (a, b) =>
      (summon[ToData[A]].toData(a), summon[ToData[B]].toData(b))
    })
  }

  given tupleToData[A: ToData, B: ToData]: ToData[(A, B)] with {
    def toData(a: (A, B)): Data =
      Constr(0, summon[ToData[A]].toData(a._1) :: summon[ToData[B]].toData(a._2) :: Nil)
  }

  given OptionToData[A: ToData]: ToData[Option[A]] with
    def toData(a: Option[A]): Data = a match
      case Some(v) => Data.Constr(0, immutable.List(v.toData))
      case None    => Data.Constr(1, immutable.List.empty)

  given EitherToData[A: ToData, B: ToData]: ToData[Either[A, B]] with
    def toData(a: Either[A, B]): Data = a match
      case Left(v)  => Data.Constr(0, immutable.List(v.toData))
      case Right(v) => Data.Constr(1, immutable.List(v.toData))

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
