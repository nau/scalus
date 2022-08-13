package scalus.uplc

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}
import org.typelevel.paiges.Doc
import scalus.uplc.Data.*
import scalus.utils.Utils

import java.util
import scala.collection.immutable

case class Constant(tpe: DefaultUni, value: Any) {
  def prettyValue: Doc =
    tpe match
      case DefaultUni.Integer => Doc.text(value.toString)
      case DefaultUni.ByteString =>
        Doc.text("#" + Utils.bytesToHex(Array.from(value.asInstanceOf[immutable.List[Byte]])))
      case DefaultUni.String => Doc.text("\"" + value.asInstanceOf[String] + "\"")
      case DefaultUni.Unit   => Doc.text("()")
      case DefaultUni.Bool   => Doc.text(if value.asInstanceOf[Boolean] then "True" else "False")
      case DefaultUni.Apply(DefaultUni.ProtoList, arg) =>
        Doc.text("[") + Doc.intercalate(
          Doc.comma,
          value.asInstanceOf[immutable.List[Constant]].map(_.prettyValue)
        ) + Doc.text("]")
      case DefaultUni.Apply(DefaultUni.Apply(DefaultUni.ProtoPair, a), b) =>
        val (x, y) = value.asInstanceOf[(Constant, Constant)]
        Doc.text("(") + x.prettyValue + Doc.comma + y.prettyValue + Doc.text(")")
      case DefaultUni.Data => Doc.text(value.toString)
      case _               => sys.error("unsupported constant type: " + tpe)
  def pretty: Doc = tpe.pretty + Doc.space + prettyValue
}

sealed abstract class Data
object Data:
  case class Constr(constr: Long, args: immutable.List[Data]) extends Data

  case class Map(values: immutable.List[(Data, Data)]) extends Data

  case class List(values: immutable.List[Data]) extends Data:
    override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"

  case class I(value: BigInt) extends Data

  case class B(value: Array[Byte]) extends Data:

    override def toString: String =
      s"B(\"${value.map("%02X" format _).mkString}\")"

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
  type T
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
  implicit object LiftBigInt extends Lift[BigInt]:
    def defaultUni: DefaultUni = DefaultUni.Integer

  implicit object LiftBoolean extends Lift[Boolean]:
    def defaultUni: DefaultUni = DefaultUni.Bool

  def defaultUniFromValue[A: Lift](value: A): DefaultUni = summon[Lift[A]].defaultUni
  def asConstant[A: Lift](value: A): Constant = Constant(defaultUniFromValue(value), value)

  case object Integer extends DefaultUni:
    type T = BigInt
  case object ByteString extends DefaultUni:
    type T = Array[Byte]
  case object String extends DefaultUni:
    type T = String
  case object Unit extends DefaultUni:
    type T = Unit

  case object Bool extends DefaultUni:
    type T = Boolean

  case object ProtoList extends DefaultUni:
    type T = Nothing // [A] =>> immutable.List[A]

  case object ProtoPair extends DefaultUni:
    type T = Nothing // [A, B] =>> (A, B)

  case class Apply(f: DefaultUni, arg: DefaultUni) extends DefaultUni:
    type T = f.T => arg.T
  case object Data extends DefaultUni:
    type T = Data

object PlutusDataCborEncoder extends Encoder[Data]:
  override def write(writer: Writer, data: Data): Writer =
    implicit val selfEncoder: Encoder[Data] = this
    data match
      case Constr(constr, args) if 0 <= constr && constr < 7 =>
        writer.writeTag(Other(121 + constr))
        writer.writeLinearSeq(args)
      case Constr(constr, args) if 7 <= constr && constr < 128 =>
        writer.writeTag(Other(1280 + (constr - 7)))
        writer.writeLinearSeq(args)
      case Constr(constr, args) =>
        writer.writeTag(Other(102))
        writer.writeArrayHeader(2)
        writer.writeLong(constr)
        writer.writeLinearSeq(args)
      case Map(values)       => writer.writeMap(values.toMap)
      case Data.List(values) => writer.writeLinearSeq(values)
      case I(value)          => writer.write(value)
      case B(value)          => writer.write(value)

object PlutusDataCborDecoder extends Decoder[Data]:

  override def read(r: Reader): Data =
    implicit val selfDecoder: Decoder[Data] = this

    val maxCborByteArraySize = 64
    def fromByteArray() =
      val byteArray = r.readByteArray()
      if byteArray.length > maxCborByteArraySize then
        r.overflow(
          "ByteArray for decoding JBigInteger is longer than the configured max of " + maxCborByteArraySize + " bytes"
        )
      else new java.math.BigInteger(1, byteArray)

    r.dataItem() match
      case DI.Int | DI.Long | DI.OverLong => I(Decoder.forBigInt.read(r))
      case DI.MapHeader                   => Map(Decoder.forMap[Data, Data].read(r).toList)
      case DI.ArrayStart | DI.ArrayHeader => Data.List(Decoder.forArray[Data].read(r).toList)
      case DI.Bytes                       => B(Decoder.forByteArray(BaseEncoding.base16).read(r))
      case DI.Tag =>
        r.readTag() match
          case Other(102) =>
            val len = r.readArrayHeader()
            val i = r.readLong()
            val args = Decoder.forArray[Data].read(r)
            Constr(i, args.toList)
          case Other(value) if 121 <= value && value < 128 =>
            Constr(value - 121, Decoder.forArray[Data].read(r).toList)
          case Other(value) if 1280 <= value && value < 1401 =>
            Constr(value - 1280 + 7, Decoder.forArray[Data].read(r).toList)
          case PositiveBigNum => I(fromByteArray())
          case NegativeBigNum => I(fromByteArray().not)
          case _              => sys.error("Unsupported") // TODO proper exception
      case i => sys.error(s"Unsupported data item $i ${DI.stringify(i)}") // TODO proper exception
