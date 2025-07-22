package scalus.cardano.plutus.contract.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import scalus.buildinfo.BuildInfo
import scalus.cardano.ledger.{Language, PlutusScript}
import scalus.cardano.ledger.Script.{PlutusV1, PlutusV2, PlutusV3}

case class Blueprint(
    preamble: Preamble,
    validators: Seq[Validator] = Nil,
) {
    def show(indentation: Int = 2): String =
        writeToString(this, WriterConfig.withIndentionStep(indentation))
}

object Blueprint {
    private[blueprint] case class Builder(
        title: String,
        description: Option[String] = None,
        validatorScript: Option[PlutusScript] = None,
        redeemerSchema: Option[PlutusDataSchema] = None,
        datumSchema: Option[PlutusDataSchema] = None,
        paramSchemas: Option[List[PlutusDataSchema]] = None
    ) {

        def withDescription(d: String): Builder = copy(description = Some(d))

        def withScript(script: PlutusScript): Builder = copy(validatorScript = Some(script))

        inline def withDatum[T]: Builder = {
            val ds = PlutusDataSchema.derived[T]
            copy(datumSchema = Some(ds))
        }

        inline def withRedeemer[T]: Builder = {
            val rs = PlutusDataSchema.derived[T]
            copy(redeemerSchema = Some(rs))
        }

        inline def withParam[T]: Builder = {
            val ps = PlutusDataSchema.derived[T]
            copy(paramSchemas = paramSchemas.map(ps :: _))
        }

        def build: Blueprint = {
            for {
                validatorScript <- this.validatorScript
                datumSchema <- this.datumSchema
                redeemerSchema <- this.redeemerSchema
            } yield {
                val preamble = Preamble(
                  title = title,
                  description = description,
                  compiler = Some(CompilerInfo("scalus", Some(BuildInfo.version))),
                  plutusVersion = Some(validatorScript.language)
                )
                val cbor = validatorScript match {
                    case PlutusV1(script) => script.toHex
                    case PlutusV2(script) => script.toHex
                    case PlutusV3(script) => script.toHex
                }
                val validator = Validator(
                  "validator",
                  compiledCode = Some(cbor),
                  hash = Some(validatorScript.scriptHash.toHex),
                  datum = Some(TypeDescription(schema = datumSchema)),
                  redeemer = Some(TypeDescription(schema = redeemerSchema)),
                  parameters = paramSchemas.map(_.map(schema => TypeDescription(schema = schema)))
                )
                Blueprint(preamble, Seq(validator))
            }
        }.get
    }

    def newBuilder(title: String): Builder = Builder(title)

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
        override def nullValue: Language = Language.PlutusV3

        override def decodeValue(in: JsonReader, default: Language): Language =
            in.readString("") match {
                case "v1" => Language.PlutusV1
                case "v2" => Language.PlutusV2
                case "v3" => Language.PlutusV3
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
                        redeemer: Option[TypeDescription] = None,
                        datum: Option[TypeDescription] = None,
                        parameters: Option[List[TypeDescription]] = None,
                        compiledCode: Option[String] = None,
                        hash: Option[String] = None
)

object Validator {
    given JsonValueCodec[Validator] = JsonCodecMaker.make
}

case class TypeDescription(
    title: Option[String] = None,
    description: Option[String] = None,
    purpose: Option[Purpose] = None,
    schema: PlutusDataSchema
)

object TypeDescription {
    given JsonValueCodec[TypeDescription] = JsonCodecMaker.make
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
    case OneOf(purposes: Seq[Purpose]) extends Purpose
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
                case "oneOf"    => OneOf(Seq.empty) // todo
            }

        def encodeValue(x: Purpose, out: JsonWriter): Unit =
            x match {
                case Spend           => out.writeVal("spend")
                case Mint            => out.writeVal("mint")
                case Withdraw        => out.writeVal("withdraw")
                case Publish         => out.writeVal("publish")
                case OneOf(purposes) =>
                    // todo
                    out.writeArrayStart()
                    out.writeArrayEnd()
            }
    }
}
