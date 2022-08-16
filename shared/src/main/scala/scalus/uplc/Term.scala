package scalus.uplc

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}
import org.typelevel.paiges.Doc
import scalus.uplc.Data.*
import scalus.utils.Utils
import scalus.utils.Utils.bytesToHex

import java.util
import scala.collection.immutable

sealed trait Constant:
  def tpe: DefaultUni

  def prettyValue: Doc

  def pretty: Doc = tpe.pretty + Doc.space + prettyValue

object Constant:

  trait LiftValue[A]:
    def lift(a: A): Constant

  given LiftValue[BigInt] with { def lift(a: BigInt): Constant = Integer(a) }
  given LiftValue[Int] with { def lift(a: Int): Constant = Integer(a) }
  given LiftValue[Array[Byte]] with { def lift(a: Array[Byte]): Constant = ByteString(a) }
  given LiftValue[java.lang.String] with { def lift(a: java.lang.String): Constant = String(a) }
  given LiftValue[Boolean] with { def lift(a: Boolean): Constant = Bool(a) }
  given LiftValue[Unit] with { def lift(a: Unit): Constant = Unit }
  given seqLiftValue[A: LiftValue: DefaultUni.Lift]: LiftValue[Seq[A]] with {
    def lift(a: Seq[A]): Constant =
      List(summon[DefaultUni.Lift[A]].defaultUni, a.map(summon[LiftValue[A]].lift).toList)
  }

  given tupleLiftValue[A: LiftValue: DefaultUni.Lift, B: LiftValue: DefaultUni.Lift]
      : LiftValue[(A, B)] with {
    def lift(a: (A, B)): Constant = Pair(
      summon[LiftValue[A]].lift(a._1),
      summon[LiftValue[B]].lift(a._2)
    )
  }

  case class Integer(value: BigInt) extends Constant:
    def tpe = DefaultUni.Integer
    def prettyValue = Doc.text(value.toString)

  case class ByteString(value: Array[Byte]) extends Constant:
    def tpe = DefaultUni.ByteString
    def prettyValue = Doc.text("#" + Utils.bytesToHex(value))

    override def equals(obj: Any): Boolean = obj match {
      case ByteString(value) => util.Arrays.equals(value, this.value)
      case _                 => false
    }

    override def hashCode(): Int = util.Arrays.hashCode(value)

  case class String(value: java.lang.String) extends Constant:
    def tpe = DefaultUni.String
    def prettyValue = Doc.text("\"" + value + "\"")

  case object Unit extends Constant:
    def tpe = DefaultUni.Unit
    def prettyValue = Doc.text("()")

  case class Bool(value: Boolean) extends Constant:
    def tpe = DefaultUni.Bool
    def prettyValue = Doc.text(if value then "True" else "False")

  case class Data(value: scalus.uplc.Data) extends Constant:
    def tpe = DefaultUni.Data
    def prettyValue = Doc.text(value.toString)

  case class List(elemType: DefaultUni, value: immutable.List[Constant]) extends Constant:
    def tpe = DefaultUni.Apply(DefaultUni.ProtoList, elemType)
    def prettyValue =
      Doc.text("[") + Doc.intercalate(Doc.text(", "), value.map(_.prettyValue)) + Doc.text("]")

  case class Pair(a: Constant, b: Constant) extends Constant:
    def tpe = DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a.tpe), b.tpe)
    def prettyValue =
      Doc.text("(") + a.prettyValue + Doc.text(", ") + b.prettyValue + Doc.text(")")

sealed abstract class Data
object Data:
  trait Lift[A]:
    def lift(a: A): Data

  given Lift[BigInt] with { def lift(a: BigInt): Data = I(a) }
  given Lift[Int] with { def lift(a: Int): Data = I(a) }
  given Lift[Array[Byte]] with { def lift(a: Array[Byte]): Data = B(a) }
  given seqLift[A: Lift]: Lift[Seq[A]] with {
    def lift(a: Seq[A]): Data = List(a.map(summon[Lift[A]].lift).toList)
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

enum Term:
  case Var(name: String) extends Term
  case LamAbs(name: String, term: Term) extends Term
  case Apply(f: Term, arg: Term) extends Term
  case Force(term: Term) extends Term
  case Delay(term: Term) extends Term
  case Const(const: Constant) extends Term
  case Builtin(bn: DefaultFun) extends Term
  case Error extends Term

  def pretty: Doc = this match
    case Var(name) => Doc.text(name)
    case LamAbs(name, term) =>
      Doc.text("(") + Doc.text("lam") + Doc.space + Doc.text(name) + Doc.space + term.pretty + Doc
        .text(")")
    case Apply(f, arg) =>
      Doc.text("[") + f.pretty + Doc.space + arg.pretty + Doc.text("]")
    case Force(term) =>
      Doc.text("(") + Doc.text("force") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Delay(term) =>
      Doc.text("(") + Doc.text("delay") + Doc.text(" ") + term.pretty + Doc.text(")")
    case Const(const) => Doc.text("(") + Doc.text("con") + Doc.space + const.pretty + Doc.text(")")
    case Builtin(bn)  => Doc.text("(") + Doc.text("builtin") + Doc.space + bn.pretty + Doc.text(")")
    case Error        => Doc.text("(error)")

object TermDSL:
  def λ(name: String)(term: Term): Term = Term.LamAbs(name, term)
  def lam(name: String)(term: Term): Term = Term.LamAbs(name, term)
  extension (term: Term)
    def $(rhs: Term) = Term.Apply(term, rhs)
    def unary_! = Term.Force(term)
    def unary_~ = Term.Delay(term)

  given Conversion[DefaultFun, Term] with
    def apply(bn: DefaultFun): Term = Term.Builtin(bn)

  given constantAsTerm[A: Constant.LiftValue]: Conversion[A, Term] with
    def apply(c: A): Term = Term.Const(summon[Constant.LiftValue[A]].lift(c))

  given Conversion[Constant, Term] with
    def apply(c: Constant): Term = Term.Const(c)

case class Program(version: (Int, Int, Int), term: Term):
  def pretty: Doc =
    val (major, minor, patch) = version
    Doc.text("(") + Doc.text("program") + Doc.space + Doc.text(
      s"$major.$minor.$patch"
    ) + Doc.space + term.pretty + Doc.text(")")

object DefaultFun1:
  lazy val cached: immutable.Map[String, DefaultFun] =
    DefaultFun.values.map(v => Utils.lowerFirst(v.toString) -> v).toMap

enum DefaultFun:
  // Integers
  case AddInteger extends DefaultFun
  case SubtractInteger extends DefaultFun
  case MultiplyInteger extends DefaultFun
  case DivideInteger extends DefaultFun
  case QuotientInteger extends DefaultFun
  case RemainderInteger extends DefaultFun
  case ModInteger extends DefaultFun
  case EqualsInteger extends DefaultFun
  case LessThanInteger extends DefaultFun
  case LessThanEqualsInteger extends DefaultFun
  // Bytestrings
  case AppendByteString extends DefaultFun
  case ConsByteString extends DefaultFun
  case SliceByteString extends DefaultFun
  case LengthOfByteString extends DefaultFun
  case IndexByteString extends DefaultFun
  case EqualsByteString extends DefaultFun
  case LessThanByteString extends DefaultFun
  case LessThanEqualsByteString extends DefaultFun
  // Cryptography and hashes
  case Sha2_256 extends DefaultFun
  case Sha3_256 extends DefaultFun
  case Blake2b_256 extends DefaultFun
  case VerifyEd25519Signature extends DefaultFun // formerly verifySignature
  case VerifyEcdsaSecp256k1Signature extends DefaultFun
  case VerifySchnorrSecp256k1Signature extends DefaultFun

  // Strings
  case AppendString extends DefaultFun
  case EqualsString extends DefaultFun
  case EncodeUtf8 extends DefaultFun
  case DecodeUtf8 extends DefaultFun

  // Bool
  case IfThenElse extends DefaultFun

  // Unit
  case ChooseUnit extends DefaultFun

  // Tracing
  case Trace extends DefaultFun

  // Pairs
  case FstPair extends DefaultFun

  case SndPair extends DefaultFun

  // Lists
  case ChooseList extends DefaultFun
  case MkCons extends DefaultFun
  case HeadList extends DefaultFun
  case TailList extends DefaultFun
  case NullList extends DefaultFun

  // Data
  // See Note [Pattern matching on built-in types].
  // It is convenient to have a "choosing" function for a data type that has more than two
  // constructors to get pattern matching over it and we may end up having multiple such data
  // types, hence we include the name of the data type as a suffix.
  case ChooseData extends DefaultFun
  case ConstrData extends DefaultFun
  case MapData extends DefaultFun
  case List extends DefaultFun
  case IData extends DefaultFun
  case BData extends DefaultFun
  case UnConstrData extends DefaultFun
  case UnMapData extends DefaultFun
  case UnListData extends DefaultFun
  case UnIData extends DefaultFun
  case UnBData extends DefaultFun
  case EqualsData extends DefaultFun
  case SerialiseData extends DefaultFun

  // Misc monomorphized constructors.
  // We could simply replace those with constants, but we use built-in functions for consistency
  // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
  // See note [Representable built-in functions over polymorphic built-in types].
  case MkPairData extends DefaultFun
  case MkNilData extends DefaultFun
  case MkNilPairData extends DefaultFun

  def name: String = Utils.lowerFirst(this.toString)

  def pretty: Doc = Doc.text(name)

sealed abstract class DefaultUni:
  type Unlifted
  def pretty: Doc = this match
    case DefaultUni.Integer    => Doc.text("integer")
    case DefaultUni.ByteString => Doc.text("bytestring")
    case DefaultUni.String     => Doc.text("string")
    case DefaultUni.Unit       => Doc.text("unit")
    case DefaultUni.Bool       => Doc.text("bool")
    case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
      Doc.text("(") + Doc.text("list") + Doc.space + arg.pretty + Doc.text(")")
    case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
      Doc.text("(") + Doc.text("pair") + Doc.space + a.pretty + Doc.space + b.pretty + Doc.text(")")
    case DefaultUni.Data => Doc.text("data")
    case _               => sys.error(s"Unexpected default uni: $this")

object DefaultUni:

  trait Lift[A]:
    def defaultUni: DefaultUni

  sealed abstract class LiftedUni[A] extends DefaultUni with Lift[A]:
    type Unlifted = A
    def defaultUni: DefaultUni = this

//  given LiftBigInt: Lift[BigInt] with
//    def defaultUni: DefaultUni = DefaultUni.Integer

  def defaultUniFromValue[A: Lift](value: A): DefaultUni = summon[Lift[A]].defaultUni
  def asConstant[A: Constant.LiftValue](value: A): Constant =
    summon[Constant.LiftValue[A]].lift(value)

  given Lift[Int] with
    def defaultUni: DefaultUni = DefaultUni.Integer

  implicit case object Integer extends LiftedUni[BigInt]
  implicit case object ByteString extends LiftedUni[Array[Byte]]
  implicit case object String extends LiftedUni[String]
  implicit case object Unit extends LiftedUni[Unit]
  implicit case object Bool extends LiftedUni[Boolean]

  case object ProtoList extends DefaultUni:
    type Unlifted = Nothing // [A] =>> immutable.List[A]

  case object ProtoPair extends DefaultUni:
    type Unlifted = Nothing // [A, B] =>> (A, B)

  case class Apply(f: DefaultUni, arg: DefaultUni) extends DefaultUni:
    type Unlifted = f.Unlifted => arg.Unlifted
  case object Data extends DefaultUni:
    type Unlifted = Data
