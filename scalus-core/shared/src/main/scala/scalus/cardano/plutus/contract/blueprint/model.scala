package scalus.cardano.plutus.contract.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.cardano.ledger.Language
import scalus.cardano.ledger.Language.{PlutusV1, PlutusV2, PlutusV3}

case class Blueprint(
    preamble: Preamble,
    validators: Seq[Validator] = Nil,
    //        definitions: Option[Map[String, PlutusDataSchema]] = None todo
) {
    def show: String = writeToString(this)
}

object Blueprint {
    given JsonValueCodec[Blueprint] = JsonCodecMaker.make
}

case class Preamble(
    title: String,
    description: Option[String] = None,
    version: Option[String] = None,
    compiler: Option[CompilerInfo] = None,
    plutusVersion: Option[Language] = None,
    license: Option[String] = None
)

object Preamble {
    given JsonValueCodec[Language] = new JsonValueCodec[Language] {
        override def nullValue: Language = PlutusV3

        override def decodeValue(in: JsonReader, default: Language): Language =
            in.readString("") match {
                case "v1" => PlutusV1
                case "v2" => PlutusV2
                case "v3" => PlutusV3
                case x =>
                    throw new RuntimeException(
                      s"Error when reading blueprint plutus version. Expected one of [v1, v2, v3], got $x"
                    )
            }

        override def encodeValue(x: Language, out: JsonWriter): Unit =
            out.writeVal(x.show)
    }
    given JsonValueCodec[Preamble] = JsonCodecMaker.make
}

extension (lang: Language) {
    def show: String = lang match {
        case Language.PlutusV1 => "v1"
        case Language.PlutusV2 => "v2"
        case Language.PlutusV3 => "v3"
    }
}

case class CompilerInfo(
    name: String,
    version: Option[String] = None
)
object CompilerInfo {
    given JsonValueCodec[CompilerInfo] = JsonCodecMaker.make
}

case class Validator(
    title: String,
    description: Option[String] = None,
    redeemer: Option[Argument] = None,
    datum: Option[Argument] = None,
    parameters: Option[List[Argument]] = None,
    compiledCode: Option[String] = None,
    hash: Option[String] = None
)

object Validator {
    given JsonValueCodec[Validator] = JsonCodecMaker.make
}

case class Argument(
    title: Option[String] = None,
    description: Option[String] = None,
    purpose: Option[Purpose] = None,
    schema: PlutusDataSchema
)

object Argument {
    given JsonValueCodec[Argument] = JsonCodecMaker.make
}

case class PlutusDataSchema(
    dataType: Option[DataType] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    anyOf: Option[List[PlutusDataSchema]] = None,
    allOf: Option[List[PlutusDataSchema]] = None,
    oneOf: Option[List[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None,
    index: Option[Int] = None,
    fields: Option[List[PlutusDataSchema]] = None
)

object PlutusDataSchema {
    given JsonValueCodec[PlutusDataSchema] =
        JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
}

enum DataType(val value: String) {
    case Integer extends DataType("integer")
    case Bytes extends DataType("bytes")
    case List extends DataType("list")
    case Map extends DataType("map")
    case Constructor extends DataType("constructor")
    case UnitBuiltin extends DataType("#unit")
    case BooleanBuiltin extends DataType("#boolean")
    case IntegerBuiltin extends DataType("#integer")
    case BytesBuiltin extends DataType("#bytes")
    case StringBuiltin extends DataType("#string")
    case PairBuiltin extends DataType("#pair")
    case ListBuiltin extends DataType("#list")
}

object DataType {
    given JsonValueCodec[DataType] = new JsonValueCodec[DataType] {
        override def nullValue: DataType = DataType.Integer

        def decodeValue(in: JsonReader, default: DataType): DataType =
            val s = in.readString(null)
            DataType.values.find(_.value == s).getOrElse {
                in.decodeError(s"unknown dataType '$s'")
            }

        def encodeValue(x: DataType, out: JsonWriter): Unit = out.writeVal(x.value)
    }
}

enum Purpose {
    case Spend
    case Mint
    case Withdraw
    case Publish
    case Oneof(purposes: Seq[Purpose]) extends Purpose
}

object Purpose {
    given JsonValueCodec[Purpose] = new JsonValueCodec[Purpose] {
        override def nullValue: Purpose = Purpose.Spend

        def decodeValue(in: JsonReader, default: Purpose): Purpose =
            val s = in.readString(null)
            s match {
                case "spend"    => Spend
                case "mint"     => Mint
                case "withdraw" => Withdraw
                case "publish"  => Publish
                case "oneOf"    => Oneof(Seq.empty) // todo
            }

        def encodeValue(x: Purpose, out: JsonWriter): Unit =
            x match {
                case Spend           => out.writeVal("spend")
                case Mint            => out.writeVal("mint")
                case Withdraw        => out.writeVal("withdraw")
                case Publish         => out.writeVal("publish")
                case Oneof(purposes) =>
                    // todo
                    out.writeArrayStart()
                    out.writeArrayEnd()
            }
    }
}
