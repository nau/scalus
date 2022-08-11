package scalus.uplc

import scalus.uplc.Cek.CekValue

import scala.collection.immutable

enum TypeScheme:
  case Result
  case Arrow(t: TypeScheme)
  case All(t: TypeScheme)

case class Runtime(
    typeScheme: TypeScheme,
    args: Seq[CekValue]
)

trait Meaning:
  def typeScheme: TypeScheme

object Meaning:
  def mkMeaning(t: TypeScheme) = new Meaning {
    def typeScheme = t
  }
  import TypeScheme._

  val AddInteger = mkMeaning(Arrow(Arrow(Result)))

object Builtins:
  val BuiltinMeanings: immutable.Map[DefaultFun, Meaning] = immutable.Map.apply(
    DefaultFun.AddInteger -> Meaning.AddInteger
  )
