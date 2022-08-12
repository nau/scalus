package scalus.uplc

import scalus.uplc.Cek.CekValue
import scalus.uplc.DefaultUni.Integer

import scala.annotation.targetName
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

enum TypeScheme:
  case Result(argType: DefaultUni)
  case Arrow(argType: DefaultUni, t: TypeScheme)
  case All(t: TypeScheme)

  def ->:(t: DefaultUni): TypeScheme = Arrow(t, this)

extension (x: DefaultUni)
  def ->:(t: DefaultUni): TypeScheme = TypeScheme.Arrow(x, TypeScheme.Result(t))

case class Runtime(
    typeScheme: TypeScheme,
    f: AnyRef
)

trait Meaning:
  def typeScheme: TypeScheme

object Meaning:
  def mkMeaning(t: TypeScheme, f: AnyRef) = Runtime(t, f)
  import TypeScheme.*

  val AddInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (x: Constant) =>
        (y: Constant) =>
          () =>
            Constant(
              DefaultUni.Integer,
              x.value.asInstanceOf[BigInt] + y.value.asInstanceOf[BigInt]
            )
    )

object Builtins:
  val BuiltinMeanings: immutable.Map[DefaultFun, Runtime] = immutable.Map.apply(
    (DefaultFun.AddInteger, Meaning.AddInteger)
  )
