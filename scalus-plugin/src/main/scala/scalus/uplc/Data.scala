package scalus.uplc

import java.util
import scala.collection.immutable
import scala.collection.immutable.List
import scalus.builtins.ByteString



sealed abstract class Data

object Data:
  case class Constr(constr: Long, args: immutable.List[Data]) extends Data

  case class Map(values: immutable.List[(Data, Data)]) extends Data

  case class List(values: immutable.List[Data]) extends Data:
    override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"

  case class I(value: BigInt) extends Data

  case class B(value: ByteString) extends Data
