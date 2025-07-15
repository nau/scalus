package scalus.cardano.plutus.contract.blueprint

import com.github.plokhotnyuk.jsoniter_scala.core.*
import com.github.plokhotnyuk.jsoniter_scala.macros.*

/** Represents the UPLC data types supported in Plutus blueprints.
  *
  * These types map directly to the Untyped Plutus Core primitives that can appear at validator
  * boundaries. Types prefixed with '#' are builtin types discouraged for outward-facing interfaces.
  */
enum DataType {
    // Primary Plutus Data types (recommended for validator interfaces)
    case integer // Signed arbitrary precision integer wrapped as iData
    case bytes // Arbitrary length byte string wrapped as bData
    case Seq // Ordered Seq of Plutus data wrapped as SeqData
    case map // Associative Seq of key-value pairs wrapped as mapData
    case constructor // Constructor with zero or more fields wrapped as constrData

    // UPLC builtin types (discouraged for public interfaces)
    case `#unit` // Builtin unit value (unary constructor)
    case `#boolean` // Builtin boolean value
    case `#integer` // Builtin signed arbitrary precision integer
    case `#bytes` // Builtin byte string
    case `#string` // Builtin UTF-8 text string
    case `#pair` // Builtin pair of Data elements
    case `#Seq` // Builtin Seq of Data elements
}

object DataType {
    given JsonValueCodec[DataType] = JsonCodecMaker.make
}

/** Represents the execution contexts where a validator can be used.
  *
  * These correspond to the different script purposes in Cardano:
  *   - spend: For spending UTxOs (most common)
  *   - mint: For minting/burning tokens
  *   - withdraw: For withdrawing from staking rewards
  *   - publish: For certificate publication
  */
enum Purpose {
    case spend
    case mint
    case withdraw
    case publish
}

object Purpose {
    given JsonValueCodec[Purpose] = JsonCodecMaker.make
}

/** Represents Plutus version compatibility.
  *
  * Indicates which version of the Plutus virtual machine the validators are designed to run on.
  */
enum PlutusVersion {
    case PlutusV1
    case PlutusV2
    case PlutusV3
}

object PlutusVersion {
    given JsonValueCodec[PlutusVersion] = JsonCodecMaker.make
}

/** Base trait for all Plutus Data Schema objects.
  *
  * A Plutus Data Schema defines the structure and validation rules for data that can be processed
  * by Plutus validators. This includes datums, redeemers, and parameters.
  */
sealed trait PlutusDataSchema

object PlutusDataSchema:
    given JsonValueCodec[PlutusDataSchema] =
        JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

/** Core schema definition with validation and metadata fields.
  *
  * This represents a complete schema definition that can validate Plutus data values. All
  * validation keywords are optional to provide flexibility in schema definition.
  *
  * @param dataType
  *   The UPLC type this schema validates (optional, defaults to opaque Plutus Data)
  * @param title
  *   Short descriptive name for UI decoration
  * @param description
  *   Detailed explanation of the schema's purpose
  * @param comment
  *   Internal comment for developers (ignored by processors)
  * @param allOf
  *   Instance must validate against ALL of these schemas
  * @param anyOf
  *   Instance must validate against AT LEAST ONE of these schemas
  * @param oneOf
  *   Instance must validate against EXACTLY ONE of these schemas
  * @param not
  *   Instance must NOT validate against this schema
  */
case class Schema(
    dataType: Option[DataType] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    @named("$comment") comment: Option[String] = None,
    allOf: Option[Seq[PlutusDataSchema]] = None,
    anyOf: Option[Seq[PlutusDataSchema]] = None,
    oneOf: Option[Seq[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None
) extends PlutusDataSchema

/** Schema for bytes data type with validation constraints.
  *
  * Validates byte strings with optional length constraints and enumeration. Enum values must be
  * hex-encoded strings.
  *
  * @param enum
  *   Allowed hex-encoded byte string values
  * @param maxLength
  *   Maximum allowed byte length (inclusive)
  * @param minLength
  *   Minimum required byte length (inclusive)
  */
case class BytesSchema(
    dataType: DataType = DataType.bytes,
    title: Option[String] = None,
    description: Option[String] = None,
    @named("$comment") comment: Option[String] = None,
    allOf: Option[Seq[PlutusDataSchema]] = None,
    anyOf: Option[Seq[PlutusDataSchema]] = None,
    oneOf: Option[Seq[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None,
    `enum`: Option[Seq[String]] = None, // Hex-encoded strings
    maxLength: Option[Int] = None, // Non-negative integer
    minLength: Option[Int] = None // Non-negative integer
) extends PlutusDataSchema

/** Schema for integer data type with numeric validation constraints.
  *
  * Validates arbitrary precision signed integers with optional bounds and divisibility constraints.
  *
  * @param multipleOf
  *   Value must be divisible by this (must be > 0)
  * @param maximum
  *   Inclusive upper bound
  * @param exclusiveMaximum
  *   Exclusive upper bound
  * @param minimum
  *   Inclusive lower bound
  * @param exclusiveMinimum
  *   Exclusive lower bound
  */
case class IntegerSchema(
    dataType: DataType = DataType.integer,
    title: Option[String] = None,
    description: Option[String] = None,
    @named("$comment") comment: Option[String] = None,
    allOf: Option[Seq[PlutusDataSchema]] = None,
    anyOf: Option[Seq[PlutusDataSchema]] = None,
    oneOf: Option[Seq[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None,
    multipleOf: Option[BigInt] = None, // Must be > 0
    maximum: Option[BigInt] = None, // Inclusive upper bound
    exclusiveMaximum: Option[BigInt] = None, // Exclusive upper bound
    minimum: Option[BigInt] = None, // Inclusive lower bound
    exclusiveMinimum: Option[BigInt] = None // Exclusive lower bound
) extends PlutusDataSchema

/** Schema for Seq data type with item validation and size constraints.
  *
  * Validates ordered Seqs of Plutus data with optional item schemas, size bounds, and uniqueness
  * requirements.
  *
  * @param items
  *   Either a single schema for all items, or positional schemas for tuple-like structures
  * @param maxItems
  *   Maximum allowed Seq size (inclusive)
  * @param minItems
  *   Minimum required Seq size (inclusive, defaults to 0)
  * @param uniqueItems
  *   Whether all Seq elements must be unique
  */
case class SeqSchema(
    dataType: DataType = DataType.Seq,
    title: Option[String] = None,
    description: Option[String] = None,
    @named("$comment") comment: Option[String] = None,
    allOf: Option[Seq[PlutusDataSchema]] = None,
    anyOf: Option[Seq[PlutusDataSchema]] = None,
    oneOf: Option[Seq[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None,
    items: Option[Either[PlutusDataSchema, Seq[PlutusDataSchema]]] =
        None, // Schema or positional schemas
    maxItems: Option[Int] = None, // Non-negative integer
    minItems: Option[Int] = None, // Non-negative integer, defaults to 0
    uniqueItems: Option[Boolean] = None // Uniqueness constraint
) extends PlutusDataSchema

/** Schema for map data type with key-value validation and size constraints.
  *
  * Validates associative Seqs (maps) of Plutus data with optional schemas for keys and values, plus
  * size constraints.
  *
  * @param keys
  *   Schema that all map keys must satisfy
  * @param values
  *   Schema that all map values must satisfy
  * @param maxItems
  *   Maximum allowed number of key-value pairs
  * @param minItems
  *   Minimum required number of key-value pairs
  */
case class MapSchema(
    dataType: DataType = DataType.map,
    title: Option[String] = None,
    description: Option[String] = None,
    @named("$comment") comment: Option[String] = None,
    allOf: Option[Seq[PlutusDataSchema]] = None,
    anyOf: Option[Seq[PlutusDataSchema]] = None,
    oneOf: Option[Seq[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None,
    keys: Option[PlutusDataSchema] = None, // Schema for all keys
    values: Option[PlutusDataSchema] = None, // Schema for all values
    maxItems: Option[Int] = None, // Non-negative integer
    minItems: Option[Int] = None // Non-negative integer
) extends PlutusDataSchema

/** Schema for constructor data type representing algebraic data types.
  *
  * Validates Plutus constructors with a specific index and field schemas. This is the primary way
  * to represent custom data types in Plutus.
  *
  * @param index
  *   The constructor index (must be non-negative, mandatory)
  * @param fields
  *   Positional schemas for constructor fields (mandatory, can be empty)
  */
case class ConstructorSchema(
    dataType: DataType = DataType.constructor,
    title: Option[String] = None,
    description: Option[String] = None,
    @named("$comment") comment: Option[String] = None,
    allOf: Option[Seq[PlutusDataSchema]] = None,
    anyOf: Option[Seq[PlutusDataSchema]] = None,
    oneOf: Option[Seq[PlutusDataSchema]] = None,
    not: Option[PlutusDataSchema] = None,
    index: Int, // Non-negative integer, mandatory
    fields: Seq[PlutusDataSchema] // Positional field schemas, mandatory
) extends PlutusDataSchema
