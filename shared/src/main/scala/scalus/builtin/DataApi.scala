package scalus.builtin

import io.bullet.borer.Cbor
import scalus.Compiler
import upickle.default.*

import java.io.InputStream
import scala.collection.mutable.ArrayBuffer

/** JSON ReadWriter for the [[Data]] type.
  */
given DataReadWriter: ReadWriter[Data] =

    given ReadWriter[Data] = DataReadWriter
    readwriter[ujson.Value].bimap(
      {
          case Data.Constr(constr, args) =>
              ujson.Obj(
                "constructor" -> writeJs(constr),
                "fields" -> ujson.Arr(ArrayBuffer.from(args.map(writeJs)))
              )
          case Data.Map(values) =>
              ujson.Obj("map" -> ujson.Arr(ArrayBuffer.from(values.map { case (k, v) =>
                  ujson.Obj("k" -> writeJs(k), "v" -> writeJs(v))
              })))
          case Data.List(values) =>
              ujson.Obj("list" -> ujson.Arr(ArrayBuffer.from(values.map(writeJs))))
          case Data.I(value) =>
              val v = if value.isValidLong then writeJs(value.toLong) else writeJs(value)
              ujson.Obj("int" -> v)
          case Data.B(value) => ujson.Obj("bytes" -> writeJs(value.toHex))
      },
      json =>
          if json.obj.get("constructor").isDefined then
              Data.Constr(
                json.obj("constructor").num.toLong,
                json.obj("fields").arr.map(f => read[Data](f)).toList
              )
          else if json.obj.get("map").isDefined then
              Data.Map(
                json.obj("map")
                    .arr
                    .map { obj =>
                        val k = read[Data](obj.obj("k"))
                        val v = read[Data](obj.obj("v"))
                        k -> v
                    }
                    .toList
              )
          else if json.obj.get("list").isDefined then
              Data.List(json.obj("list").arr.map(e => read[Data](e)).toList)
          else if json.obj.get("int").isDefined then Data.I(json.obj("int").num.toLong)
          else if json.obj.get("bytes").isDefined then
              Data.B(ByteString.fromHex(json.obj("bytes").str))
          else throw new Exception("Invalid Data")
    )

trait DataApi {
    extension [A <: Data: Writer](a: A)
        /** Encode a [[Data]] value to JSON */
        inline def toJson: String = write(a)

        /** Encode a [[Data]] value to indented JSON */
        inline def toJsonIndented(indent: Int): String = write(a, indent)

    extension [A <: Data](a: A)
        /** Encode a [[Data]] value to CBOR.
          */
        def toCbor: Array[Byte] = Cbor.encode(a)(using dataCborEncoder).toByteArray

        /** Encode a [[Data]] value to CBOR [[ByteString]]
          */
        def toCborByteString: ByteString = ByteString.fromArray(toCbor)

    extension (inline data: Data)
        inline def field[A](inline expr: A => Any): Data = Compiler.fieldAsData(expr)(data)
        inline def toConstr: Pair[BigInt, scalus.builtin.List[Data]] = Builtins.unConstrData(data)
        inline def toMap: List[Pair[Data, Data]] = Builtins.unMapData(data)
        inline def toList: List[Data] = Builtins.unListData(data)
        inline def toI: BigInt = Builtins.unIData(data)
        inline def toBigInt: BigInt = Builtins.unIData(data)
        inline def toB: ByteString = Builtins.unBData(data)
        inline def toByteString: ByteString = Builtins.unBData(data)

    /** Decode a [[Data]] value from JSON */
    def fromJson(json: String): Data = read[Data](json)

    /** Encode a [[Data]] value to JSON */
    def toJson(data: Data, indent: Int = -1): String = write(data, indent)

    /** Decode a [[Data]] value from CBOR */
    def fromCbor(bytes: Array[Byte]): Data = Cbor.decode(bytes).to[Data].value

    /** Decode a [[Data]] value from CBOR */
    def fromCbor(is: InputStream): Data = Cbor.decode(is).to[Data].value

    /** Decode a [[Data]] value from CBOR */
    def fromCbor(bs: ByteString): Data = Cbor.decode(bs.bytes).to[Data].value

}
