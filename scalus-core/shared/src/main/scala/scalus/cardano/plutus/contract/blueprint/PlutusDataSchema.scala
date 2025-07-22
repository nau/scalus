package scalus.cardano.plutus.contract.blueprint
import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, WriterConfig, writeToString}
import com.github.plokhotnyuk.jsoniter_scala.macros.{CodecMakerConfig, JsonCodecMaker}
import scalus.cardano.plutus.contract.blueprint.PlutusDataSchema.given_JsonValueCodec_PlutusDataSchema

import scala.annotation.tailrec
import scala.quoted.*

case class PlutusDataSchema(
    dataType: Option[DataType] = None,
    title: Option[String] = None,
    description: Option[String] = None,
    anyOf: Option[List[PlutusDataSchema]] = None,
    index: Option[Int] = None,
    fields: Option[List[PlutusDataSchema]] = None
) {
    def show(indentation: Int = 2): String =
        writeToString(this, WriterConfig.withIndentionStep(indentation))
}

object PlutusDataSchema {
    inline def derived[T]: PlutusDataSchema = ${ deriveSchemaImpl[T] }

    private def deriveSchemaImpl[T: Type](using Quotes): Expr[PlutusDataSchema] =
        import quotes.reflect.*

        val tpe = TypeRepr.of[T].dealias.widen
        val symbol = tpe.typeSymbol

        if isPrimitive(tpe) then {
            deriveForPrimitive(tpe)
        } else if symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Enum) then {
            deriveForCaseClass(symbol)
        } else if !symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum) then {
            deriveForEnumRoot(symbol)
        } else if symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum) then {
            generateForEnumLeafWithIndex(symbol, 0)
        } else {
            report.errorAndAbort(s"Unsupported type for schema generation: ${tpe.show}")
        }

    private def deriveForPrimitive(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Expr[PlutusDataSchema] =
        import quotes.reflect.*
        tpe.show match {
            case "scala.Int" | "scala.Long" | "scala.BigInt" =>
                '{ PlutusDataSchema(dataType = Some(DataType.Integer)) }
            case "scala.Array[scala.Byte]" | "scala.collection.immutable.List[scala.Byte]" =>
                '{ PlutusDataSchema(dataType = Some(DataType.Bytes)) }
            case "scala.Boolean" =>
                '{ PlutusDataSchema(dataType = Some(DataType.BooleanBuiltin)) }
            case "java.lang.String" =>
                '{ PlutusDataSchema(dataType = Some(DataType.StringBuiltin)) }
            case _ if tpe.typeSymbol.name == "List" =>
                '{ PlutusDataSchema(dataType = Some(DataType.List)) }
            case _ if tpe.typeSymbol.name == "Map" =>
                '{ PlutusDataSchema(dataType = Some(DataType.Map)) }
            case _ =>
                report.errorAndAbort(s"Unsupported primitive type: ${tpe.show}")
        }

    private def deriveForCaseClass(using
        Quotes
    )(symbol: quotes.reflect.Symbol): Expr[PlutusDataSchema] =

        val params = getPrimaryConstructorParams(symbol)
        val fieldSchemas = params.map { case (name, typeRepr) =>
            generateFieldSchema(name, typeRepr)
        }

        val fieldsExpr = Expr.ofList(fieldSchemas)

        '{
            PlutusDataSchema(
              dataType = Some(DataType.Constructor),
              title = Some(${ Expr(symbol.name) }),
              fields = Some($fieldsExpr)
            )
        }

    private def deriveForEnumRoot(using
        Quotes
    )(symbol: quotes.reflect.Symbol): Expr[PlutusDataSchema] =
        import quotes.reflect.*

        val cases = symbol.children.filter(_.flags.is(Flags.Case))
        val caseSchemas = cases.zipWithIndex.map { case (caseSymbol, index) =>
            generateForEnumLeafWithIndex(caseSymbol, index)
        }

        val anyOfExpr = Expr.ofList(caseSchemas)

        '{
            PlutusDataSchema(
              title = Some(${ Expr(symbol.name) }),
              anyOf = Some($anyOfExpr)
            )
        }

    private def generateForEnumLeafWithIndex(using
        Quotes
    )(symbol: quotes.reflect.Symbol, index: Int = 0): Expr[PlutusDataSchema] =

        val params = getPrimaryConstructorParams(symbol)

        if params.isEmpty then {
            '{
                PlutusDataSchema(
                  dataType = Some(DataType.Constructor),
                  title = Some(${ Expr(symbol.name) }),
                  index = Some(${ Expr(index) }),
                  fields = Some(List.empty)
                )
            }
        } else {
            val fieldSchemas = params.map { case (name, typeRepr) =>
                generateFieldSchema(name, typeRepr)
            }
            val fieldsExpr = Expr.ofList(fieldSchemas)

            '{
                PlutusDataSchema(
                  dataType = Some(DataType.Constructor),
                  title = Some(${ Expr(symbol.name) }),
                  index = Some(${ Expr(index) }),
                  fields = Some($fieldsExpr)
                )
            }
        }

    private def generateFieldSchema(using
        Quotes
    )(name: String, tpe: quotes.reflect.TypeRepr): Expr[PlutusDataSchema] =

        val dataType = resolveFieldDataType(tpe)

        '{
            PlutusDataSchema(
              dataType = $dataType,
              title = Some(${ Expr(name) })
            )
        }

    @tailrec
    private def resolveFieldDataType(using
        Quotes
    )(tpe: quotes.reflect.TypeRepr): Expr[Option[DataType]] =
        import quotes.reflect.*

        if isPrimitive(tpe) then {
            tpe.show match {
                case "scala.Int" | "scala.Long" | "scala.math.BigInt" =>
                    '{ Some(DataType.Integer) }
                case "scala.Array[scala.Byte]" | "scalus.builtin.ByteString" =>
                    '{ Some(DataType.Bytes) }
                case "scala.Boolean" =>
                    '{ Some(DataType.BooleanBuiltin) }
                case "java.lang.String" =>
                    '{ Some(DataType.StringBuiltin) }
                case _ if tpe.typeSymbol.name == "List" =>
                    '{ Some(DataType.List) }
                case _ if tpe.typeSymbol.name == "Map" =>
                    '{ Some(DataType.Map) }
                case _ =>
                    '{ Some(DataType.Constructor) }
            }
        } else {
            val symbol = tpe.typeSymbol
            if symbol.flags.is(Flags.Case) && !symbol.flags.is(Flags.Enum) then {
                val params = getPrimaryConstructorParams(symbol)
                if params.length == 1 then {
                    val (_, fieldType) = params.head
                    if isPrimitive(fieldType) then {
                        resolveFieldDataType(fieldType)
                    } else {
                        '{ Some(DataType.Constructor) }
                    }
                } else {
                    '{ Some(DataType.Constructor) }
                }
            } else {
                '{ Some(DataType.Constructor) }
            }
        }

    private def isPrimitive(using Quotes)(tpe: quotes.reflect.TypeRepr): Boolean =

        tpe.show match {
            case "scala.Int" | "scala.Long" | "scala.math.BigInt" | "scala.Boolean" |
                "java.lang.String" =>
                true
            case "scala.Array[scala.Byte]" | "scalus.builtin.ByteString"            => true
            case _ if tpe.typeSymbol.name == "List" || tpe.typeSymbol.name == "Map" => true
            case _                                                                  => false
        }

    private def isCaseClassOrEnum(using Quotes)(symbol: quotes.reflect.Symbol): Boolean =
        import quotes.reflect.*

        val isCaseClass = symbol.isClassDef && symbol.flags.is(Flags.Case)
        val isEnum = symbol.isClassDef && symbol.flags.is(Flags.Enum)
        val isEnumCase =
            symbol.isClassDef && symbol.flags.is(Flags.Case) && symbol.flags.is(Flags.Enum)

        isCaseClass || isEnum || isEnumCase

    private def getConstructorParams(using
        Quotes
    )(symbol: quotes.reflect.Symbol): List[(String, quotes.reflect.TypeRepr)] =
        import quotes.reflect.*

        if symbol.flags.is(Flags.Enum) && !symbol.flags.is(Flags.Case) then
            symbol.children.filter(_.flags.is(Flags.Case)).flatMap { caseSymbol =>
                getPrimaryConstructorParams(caseSymbol)
            }
        else getPrimaryConstructorParams(symbol)

    private def getPrimaryConstructorParams(using
        Quotes
    )(symbol: quotes.reflect.Symbol): List[(String, quotes.reflect.TypeRepr)] =
        import quotes.reflect.*

        val primaryConstructor = symbol.primaryConstructor
        primaryConstructor.paramSymss.flatten.map { param =>
            val paramName = param.name
            val paramType = param.tree match
                case ValDef(_, tpt, _) =>
                    tpt.tpe.dealias.widen
                case _ =>
                    TypeRepr.of[Nothing]

            (paramName, paramType)
        }

    given JsonValueCodec[PlutusDataSchema] =
        JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))
}
