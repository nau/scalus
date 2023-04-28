package scalus.uplc

import scalus.builtins

import java.util
import scala.collection.immutable

sealed trait Constant

object Constant:

  case class Integer(value: BigInt) extends Constant

  case class ByteString(value: builtins.ByteString) extends Constant

  case class String(value: java.lang.String) extends Constant

  case object Unit extends Constant

  case class Bool(value: Boolean) extends Constant

  case class Data(value: scalus.uplc.Data) extends Constant

  case class List(elemType: DefaultUni, value: immutable.List[Constant]) extends Constant

  case class Pair(a: Constant, b: Constant) extends Constant

