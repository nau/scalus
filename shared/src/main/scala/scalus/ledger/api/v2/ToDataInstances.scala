package scalus.ledger.api.v2

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins
import scalus.ledger.api.v1.Datum
import scalus.ledger.api.v1.DatumHash
import scalus.builtin.Data
import scalus.builtin.Data.ToData
import scalus.builtin.ToData

@Compile
object ToDataInstances {
    import scalus.builtin.Data.toData
    import scalus.builtin.ToDataInstances.given

    given OutputDatumToData[T <: OutputDatum]: ToData[T] = (d: T) =>
        d match
            case OutputDatum.NoOutputDatum => Builtins.constrData(0, Builtins.mkNilData())
            case OutputDatum.OutputDatumHash(datumHash) =>
                Builtins.constrData(1, builtin.List(datumHash.toData))
            case OutputDatum.OutputDatum(datum) =>
                Builtins.constrData(2, builtin.List(datum.toData))

    given ToData[TxOut] = ToData.deriveCaseClass[TxOut](0)

    given ToData[TxInInfo] = ToData.deriveCaseClass[TxInInfo](0)

    given ToData[TxInfo] = ToData.deriveCaseClass[TxInfo](0)

    given ToData[ScriptContext] = ToData.deriveCaseClass[ScriptContext](0)
}
