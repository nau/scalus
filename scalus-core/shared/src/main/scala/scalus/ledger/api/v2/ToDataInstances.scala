package scalus.ledger.api.v2

@deprecated("Don't need anymore, use companion objects of appropriative types instead")
object ToDataInstances {

    // given OutputDatumToData[T <: OutputDatum]: ToData[T] = (d: T) =>
    //    d match
    //        case OutputDatum.NoOutputDatum => constrData(0, mkNilData())
    //        case OutputDatum.OutputDatumHash(datumHash) =>
    //            constrData(1, builtin.List(datumHash.toData))
    //        case OutputDatum.OutputDatum(datum) =>
    //            constrData(2, builtin.List(datum))

    // given ToData[TxOut] = ToData.deriveCaseClass[TxOut](0)

    // given ToData[TxInInfo] = ToData.deriveCaseClass[TxInInfo](0)

    // given ToData[TxInfo] = ToData.deriveCaseClass[TxInfo](0)

    // given ToData[ScriptContext] = ToData.deriveCaseClass[ScriptContext](0)
}
