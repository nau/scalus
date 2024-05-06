package scalus.bloxbean

import com.bloxbean.cardano.client.plutus.spec.*
import com.bloxbean.cardano.client.plutus.spec.PlutusData
import scalus.builtin.ByteString
import scalus.builtin.Data

import scala.jdk.CollectionConverters.*

/** Interoperability between Cardano Client Lib and Scalus */
object Interop {

    /** Converts Cardano Client Lib's [[PlutusData]] to Scalus' [[Data]] */
    def toScalusData(datum: PlutusData): Data =
        datum match
            case c: ConstrPlutusData =>
                val constr = c.getAlternative()
                val args = c.getData().getPlutusDataList().asScala.map(toScalusData).toList
                Data.Constr(c.getAlternative(), args)
            case m: MapPlutusData =>
                val values = m
                    .getMap()
                    .asScala
                    .map { case (k, v) => (toScalusData(k), toScalusData(v)) }
                    .toList
                Data.Map(values)
            case l: ListPlutusData =>
                val values = l.getPlutusDataList().asScala.map(toScalusData).toList
                Data.List(values)
            case i: BigIntPlutusData =>
                Data.I(i.getValue())
            case b: BytesPlutusData =>
                Data.B(ByteString.fromArray(b.getValue()))

    /** Converts Scalus' [[Data]] to Cardano Client Lib's [[PlutusData]] */
    def toPlutusData(data: Data): PlutusData =
        data match
            case Data.Constr(tag, args) =>
                val convertedArgs = ListPlutusData
                    .builder()
                    .plutusDataList(args.map(toPlutusData).asJava)
                    .build()
                ConstrPlutusData
                    .builder()
                    .alternative(tag)
                    .data(convertedArgs)
                    .build()
            case Data.Map(items) =>
                MapPlutusData
                    .builder()
                    .map(
                      items
                          .map { case (k, v) =>
                              (toPlutusData(k), toPlutusData(v))
                          }
                          .toMap
                          .asJava
                    )
                    .build()
            case Data.List(items) =>
                ListPlutusData
                    .builder()
                    .plutusDataList(items.map(toPlutusData).asJava)
                    .build()
            case Data.I(i) =>
                BigIntPlutusData.of(i.bigInteger)
            case Data.B(b) =>
                BytesPlutusData.of(b.bytes)
}
