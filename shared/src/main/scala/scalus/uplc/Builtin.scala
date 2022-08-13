package scalus.uplc

import scalus.uplc.Cek.CekValue
import scalus.uplc.DefaultUni.{Bool, Integer}

import scala.annotation.targetName
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

enum TypeScheme:
  case Type(argType: DefaultUni)
  case Arrow(argType: TypeScheme, t: TypeScheme)
  case All(name: String, t: TypeScheme)
  case TVar(name: String)

  def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)

  def unifiesWith(t: DefaultUni): Boolean = this match
    case Type(t2)   => t2 == t
    case TVar(name) => true
    case _          => false

extension (x: DefaultUni)
  def ->:(t: DefaultUni): TypeScheme = TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))
  def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))

case class Runtime(
    typeScheme: TypeScheme,
    f: AnyRef
)

trait Meaning:
  def typeScheme: TypeScheme

object Meaning:
  def mkMeaning(t: TypeScheme, f: AnyRef) = Runtime(t, f)
  import TypeScheme.*

  def check[A](t: DefaultUni, v: CekValue)(f: A => CekValue): CekValue =
    v match
      case Cek.VCon(Constant(tpe, value)) if t == tpe => f(value.asInstanceOf[A])
      case _ =>
        sys.error("Unexpected value: " + v)

  val AddInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (x: CekValue) => (y: CekValue) => () => x
    )

  val IfThenElse =
    mkMeaning(
      All("a", Bool ->: TVar("a") ->: TVar("a")),
      (b: CekValue) => (t: CekValue) => (f: CekValue) => () => t
    )

object Builtins:
  val BuiltinMeanings: immutable.Map[DefaultFun, Runtime] = immutable.Map.apply(
    (DefaultFun.AddInteger, Meaning.AddInteger),
    (DefaultFun.IfThenElse, Meaning.IfThenElse)
  )
