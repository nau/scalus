package scalus.uplc

import scalus.uplc.Cek.CekValue
import scalus.uplc.DefaultUni.{Bool, Integer, asConstant, fromConstant}

import scala.annotation.targetName
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer

enum TypeScheme:
  case Type(argType: DefaultUni)
  case Arrow(argType: TypeScheme, t: TypeScheme)
  case All(name: String, t: TypeScheme)
  case TVar(name: String)

  def ->:(t: TypeScheme): TypeScheme = Arrow(t, this)
  def ->:(t: DefaultUni): TypeScheme = Arrow(Type(t), this)

  def unifiesWith(t: DefaultUni): Boolean = this match
    case Type(t2)   => t2 == t
    case TVar(name) => true
    case _          => false

extension (x: DefaultUni)
  def ->:(t: DefaultUni): TypeScheme = TypeScheme.Arrow(TypeScheme.Type(t), TypeScheme.Type(x))

//  def ->:(t: TypeScheme): TypeScheme = TypeScheme.Arrow(t, TypeScheme.Type(x))

case class Runtime(
    typeScheme: TypeScheme,
    f: AnyRef
)

trait Meaning:
  def typeScheme: TypeScheme

object Meaning:
  def mkMeaning(t: TypeScheme, f: AnyRef) = Runtime(t, f)
  import TypeScheme.*

  def check(t: DefaultUni, v: CekValue): t.Unlifted =
    v match
      case Cek.VCon(c: Constant) if t == c.tpe => c.value.asInstanceOf[t.Unlifted]
      case _ =>
        sys.error("Unexpected value: " + v)

  val AddInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = check(Integer, a)
        (b: CekValue) =>
          val bb = check(Integer, b)
          () => Cek.VCon(asConstant(aa + bb))
    )
  val MultiplyInteger =
    mkMeaning(
      Integer ->: Integer ->: Integer,
      (a: CekValue) =>
        val aa = check(Integer, a)
        (b: CekValue) =>
          val bb = check(Integer, b)
          () => Cek.VCon(asConstant(aa * bb))
    )
  val EqualsInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val aa = check(Integer, a)
        (b: CekValue) =>
          val bb = check(Integer, b)
          () => Cek.VCon(asConstant(aa == bb))
    )
  val LessThanEqualsInteger =
    mkMeaning(
      Integer ->: Integer ->: Bool,
      (a: CekValue) =>
        val aa = check(Integer, a)
        (b: CekValue) =>
          val bb = check(Integer, b)
          () => Cek.VCon(asConstant(aa <= bb))
    )

  val IfThenElse =
    mkMeaning(
      All("a", Bool ->: TVar("a") ->: TVar("a") ->: TVar("a")),
      (b: CekValue) =>
        val bb = check(Bool, b)
        (t: CekValue) => (f: CekValue) => () => if bb then t else f
    )

  val BuiltinMeanings: immutable.Map[DefaultFun, Runtime] = immutable.Map.apply(
    (DefaultFun.AddInteger, Meaning.AddInteger),
    (DefaultFun.MultiplyInteger, Meaning.MultiplyInteger),
    (DefaultFun.EqualsInteger, Meaning.EqualsInteger),
    (DefaultFun.LessThanEqualsInteger, Meaning.LessThanEqualsInteger),
    (DefaultFun.IfThenElse, Meaning.IfThenElse)
  )
