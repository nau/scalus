package scalus.cardano.plutus.contract.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.buildinfo.BuildInfo
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.ledger.ScriptHash
import scalus.cardano.plutus.contract.blueprint.model.PlutusVersion.{v1, v2, v3}
import scalus.cardano.plutus.contract.blueprint.model.Purpose.*
import scalus.prelude.Validator as ScalusValidator
import scalus.{plutusV3, toUplcOptimized, Compiler}
import scalus.utils.Hex.toHex
object model {

    def mkBlueprint(
        contractTitle: String,
        description: String,
        version: PlutusVersion,
        validator: ScalusValidator
    ) = {
        val preamble = mkPreamble(contractTitle, description, version)
        val blueprintValidator = mkValidator(version, validator)
        Blueprint(preamble, Seq(blueprintValidator))
    }

    def blueprintString(
        contractTitle: String,
        description: String,
        version: PlutusVersion,
        validator: ScalusValidator
    ) = writeToString(mkBlueprint(contractTitle, description, version, validator))

    private def mkPreamble(title: String, description: String, version: PlutusVersion) =
        Preamble(
          title = title,
          description = Some(description),
          compiler = Some(CompilerInfo("scalus", Some(BuildInfo.scalusVersion))),
          plutusVersion = Some(version)
        )

    private inline def mkValidator(version: PlutusVersion, validator: ScalusValidator) = {
        val title = validator.getClass.getSimpleName
        val uplc = validatorUplc(validator.validate).plutusV3
        val cboredFn = uplc.cborEncoded.toHex
        val preimage = version.langTag.toByte +: uplc.flatEncoded
        val hash = platform.blake2b_224(ByteString.fromArray(preimage)).toHex

        Validator(
          title,
          compiledCode = Some(cboredFn),
          hash = Some(hash)
        )
    }

    private type ValidatorFn = Data => Unit

    private inline def validatorUplc(inline fn: ValidatorFn) =
        Compiler.compileInline(fn).toUplcOptimized()

    case class Preamble(
        title: String,
        description: Option[String] = None,
        version: Option[String] = None,
        compiler: Option[CompilerInfo] = None,
        plutusVersion: Option[PlutusVersion] = None,
        license: Option[String] = None
    )

    case class CompilerInfo(
        name: String,
        version: Option[String] = None
    )

    case class Blueprint(
        preamble: Preamble,
        validators: Seq[Validator] = Nil,
        //        definitions: Option[Map[String, PlutusDataSchema]] = None
    )

    case class Validator(
        title: String,
        description: Option[String] = None,
        redeemer: Option[Argument] = None,
        datum: Option[Argument] = None,
        parameters: Option[List[Argument]] = None,
        compiledCode: Option[String] = None,
        hash: Option[String] = None
    )

    case class Argument(
        title: Option[String] = None,
        description: Option[String] = None,
        purpose: Option[Purpose] = None,
        schema: PlutusDataSchema
    )

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

    enum Purpose {
        case Spend
        case Mint
        case Withdraw
        case Publish
        case Oneof(purposes: Seq[Purpose]) extends Purpose
    }

    enum PlutusVersion {
        case v1
        case v2
        case v3

        def langTag = this match {
            case PlutusVersion.v1 => 0x01
            case PlutusVersion.v2 => 0x02
            case PlutusVersion.v3 => 0x03
        }
    }

    given JsonValueCodec[PlutusVersion] = new JsonValueCodec[PlutusVersion] {
        override def nullValue: PlutusVersion = v3

        def decodeValue(in: JsonReader, default: PlutusVersion): PlutusVersion = {
            val s = in.readString(null)
            s match {
                case "v1" => v1
                case "v2" => v2
                case "v3" => v3
                case _    => v3
            }
        }

        def encodeValue(x: PlutusVersion, out: JsonWriter): Unit = {
            val strValue = x match {
                case v1 => "v1"
                case v2 => "v2"
                case v3 => "v3"
            }
            out.writeVal(strValue)
        }
    }

    given JsonValueCodec[CompilerInfo] = JsonCodecMaker.make

    given JsonValueCodec[Preamble] = JsonCodecMaker.make

    given JsonValueCodec[Blueprint] = JsonCodecMaker.make

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

    given JsonValueCodec[DataType] = new JsonValueCodec[DataType] {
        override def nullValue: DataType = DataType.Integer

        def decodeValue(in: JsonReader, default: DataType): DataType =
            val s = in.readString(null)
            DataType.values.find(_.value == s).getOrElse {
                in.decodeError(s"unknown dataType '$s'")
            }

        def encodeValue(x: DataType, out: JsonWriter): Unit = out.writeVal(x.value)
    }

    given JsonValueCodec[PlutusDataSchema] =
        JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

    given JsonValueCodec[Argument] = JsonCodecMaker.make

    given JsonValueCodec[Validator] = JsonCodecMaker.make

}

//    case class Blueprint(
//        preamble: Preamble,
//        validators: List[Validator],
//        definitions: Option[Map[String, PlutusDataSchema]] = None
//    ) {
//
//        def toJson: String = Blueprint.write(this)
//    }
//
//    object Blueprint {
//
//        def fromJson(json: String): Try[Blueprintt] =
//            Try(readFromString[Blueprintt](json))
//
//        given JsonValueCodec[DataType] = new JsonValueCodec[DataType] {
//            override def nullValue: DataType = DataType.Integer
//
//            def decodeValue(in: JsonReader, default: DataType): DataType =
//                val s = in.readString(null)
//                DataType.values.find(_.value == s).getOrElse {
//                    in.decodeError(s"unknown dataType '$s'")
//                }
//
//            def encodeValue(x: DataType, out: JsonWriter): Unit = out.writeVal(x.value)
//        }
//

//
//        given JsonValueCodec[Argument] = JsonCodecMaker.make
//
//        given JsonValueCodec[Validator] = JsonCodecMaker.make
//
//
//        // helpers re‑exposed for users
//        private def write(b: Blueprint): String = writeToString(b)
//    }
//
//
//
//    enum PlutusVersion:
//        case V1
//        case V2
//        case V3
//
//
//
