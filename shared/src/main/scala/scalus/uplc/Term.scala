package scalus.uplc

import io.bullet.borer.Tag.{NegativeBigNum, Other, PositiveBigNum}
import io.bullet.borer.encodings.BaseEncoding
import io.bullet.borer.{Decoder, Encoder, Reader, Writer, DataItem as DI}

import java.util
import scala.collection.immutable

case class Constant(tpe: DefaultUni, value: Any)

sealed trait Data
case class Constr(constr: Long, args: immutable.List[Data]) extends Data
case class Map(values: immutable.List[(Data, Data)]) extends Data
case class List(values: immutable.List[Data]) extends Data {
  override def toString: String = s"List(${values.map(v => v.toString + "::").mkString}Nil)"
}
case class I(value: BigInt) extends Data
case class B(value: Array[Byte]) extends Data {

  override def toString: String = {
    s"B(\"${value.map("%02X" format _).mkString}\")"
  }

  override def equals(that: Any): Boolean = that match {
    case that: B =>
      that.canEqual(this) &&
      util.Arrays.equals(value, that.value)
    case _ => false
  }

  // Step 8 - implement a corresponding hashCode c=method
  override def hashCode: Int = util.Arrays.hashCode(value)
}

sealed trait Term
case class Var(name: String) extends Term
case class LamAbs(name: String, term: Term) extends Term
case class Apply(f: Term, arg: Term) extends Term
case class Force(term: Term) extends Term
case class Delay(term: Term) extends Term
case class Const(const: Constant) extends Term
case class Builtin(bn: DefaultFun) extends Term
case object Error extends Term

case class Program(version: (Int, Int, Int), term: Term)

sealed trait DefaultFun {}
// Integers
case object AddInteger extends DefaultFun
case object SubtractInteger extends DefaultFun
case object MultiplyInteger extends DefaultFun
case object DivideInteger extends DefaultFun
case object QuotientInteger extends DefaultFun
case object RemainderInteger extends DefaultFun
case object ModInteger extends DefaultFun
case object EqualsInteger extends DefaultFun
case object LessThanInteger extends DefaultFun
case object LessThanEqualsInteger extends DefaultFun
// Bytestrings
case object AppendByteString extends DefaultFun
case object ConsByteString extends DefaultFun
case object SliceByteString extends DefaultFun
case object LengthOfByteString extends DefaultFun
case object IndexByteString extends DefaultFun
case object EqualsByteString extends DefaultFun
case object LessThanByteString extends DefaultFun
case object LessThanEqualsByteString extends DefaultFun
// Cryptography and hashes
case object Sha2_256 extends DefaultFun
case object Sha3_256 extends DefaultFun
case object Blake2b_256 extends DefaultFun
case object VerifyEd25519Signature extends DefaultFun // formerly verifySignature
case object VerifyEcdsaSecp256k1Signature extends DefaultFun
case object VerifySchnorrSecp256k1Signature extends DefaultFun
// Strings
case object AppendString extends DefaultFun
case object EqualsString extends DefaultFun
case object EncodeUtf8 extends DefaultFun
case object DecodeUtf8 extends DefaultFun
// Bool
case object IfThenElse extends DefaultFun
// Unit
case object ChooseUnit extends DefaultFun
// Tracing
case object Trace extends DefaultFun
// Pairs
case object FstPair extends DefaultFun
case object SndPair extends DefaultFun
// Lists
case object ChooseList extends DefaultFun
case object MkCons extends DefaultFun
case object HeadList extends DefaultFun
case object TailList extends DefaultFun
case object NullList extends DefaultFun
// Data
// See Note [Pattern matching on built-in types].
// It is convenient to have a "choosing" function for a data type that has more than two
// constructors to get pattern matching over it and we may end up having multiple such data
// types, hence we include the name of the data type as a suffix.
case object ChooseData extends DefaultFun
case object ConstrData extends DefaultFun
case object MapData extends DefaultFun
case object List extends DefaultFun
case object IData extends DefaultFun
case object BData extends DefaultFun
case object UnConstrData extends DefaultFun
case object UnMapData extends DefaultFun
case object UnListData extends DefaultFun
case object UnIData extends DefaultFun
case object UnBData extends DefaultFun
case object EqualsData extends DefaultFun
case object SerialiseData extends DefaultFun
// Misc monomorphized constructors.
// We could simply replace those with constants, but we use built-in functions for consistency
// with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
// See note [Representable built-in functions over polymorphic built-in types].
case object MkPairData extends DefaultFun
case object MkNilData extends DefaultFun
case object MkNilPairData extends DefaultFun

sealed trait DefaultUni
case object DefaultUniInteger extends DefaultUni
case object DefaultUniByteString extends DefaultUni
case object DefaultUniString extends DefaultUni
case object DefaultUniUnit extends DefaultUni
case object DefaultUniBool extends DefaultUni
case object DefaultUniProtoList extends DefaultUni
case object DefaultUniProtoPair extends DefaultUni
case class DefaultUniApply(f: DefaultUni, arg: DefaultUni) extends DefaultUni
case object DefaultUniData extends DefaultUni

object PlutusDataCborEncoder extends Encoder[Data] {
  override def write(writer: Writer, data: Data): Writer = {
    implicit val selfEncoder: Encoder[Data] = this
    data match {
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
      case Map(values)  => writer.writeMap(values.toMap)
      case List(values) => writer.writeLinearSeq(values)
      case I(value)     => writer.write(value)
      case B(value)     => writer.write(value)
    }
  }
}

object PlutusDataCborDecoder extends Decoder[Data] {

  override def read(r: Reader): Data = {
    implicit val selfDecoder: Decoder[Data] = this

    val maxCborByteArraySize = 64
    def fromByteArray() = {
      val byteArray = r.readByteArray()
      if (byteArray.length > maxCborByteArraySize) {
        r.overflow(
          "ByteArray for decoding JBigInteger is longer than the configured max of " + maxCborByteArraySize + " bytes"
        )
      } else new java.math.BigInteger(1, byteArray)
    }

    r.dataItem() match {
      case DI.Int | DI.Long | DI.OverLong => I(Decoder.forBigInt.read(r))
      case DI.MapHeader                   => Map(Decoder.forMap[Data, Data].read(r).toList)
      case DI.ArrayStart | DI.ArrayHeader => List(Decoder.forArray[Data].read(r).toList)
      case DI.Bytes                       => B(Decoder.forByteArray(BaseEncoding.base16).read(r))
      case DI.Tag =>
        r.readTag() match {
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
        }
      case i => sys.error(s"Unsupported data item $i ${DI.stringify(i)}") // TODO proper exception
    }

  }

}
