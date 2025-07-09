package scalus.ledger.api.v2

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins.*
import scalus.builtin.Data
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.ledger.api.v2.OutputDatum.NoOutputDatum
import scalus.prelude.AssocMap
import scalus.prelude.List
import scalus.prelude.Option
import scalus.prelude.Eq
import scalus.builtin.ByteString.*

@deprecated("Don't need anymore, use companion objects of appropriative types instead")
object FromDataInstances {

    // given FromData[OutputDatum] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val tag = pair.fst
    //    val args = pair.snd
    //    if tag == BigInt(0) then OutputDatum.NoOutputDatum
    //    else if tag == BigInt(1) then
    //        new OutputDatum.OutputDatumHash(fromData[DatumHash](args.head))
    //    else if tag == BigInt(2) then new OutputDatum.OutputDatum(fromData[Datum](args.head))
    //    else throw new Exception("PT1")

    // given FromData[TxOut] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val args = pair.snd
    //    new TxOut(
    //      fromData[Address](args.head),
    //      fromData[Value](args.tail.head),
    //      fromData[OutputDatum](args.tail.tail.head),
    //      fromData[Option[ScriptHash]](args.tail.tail.tail.head)
    //    )

    // given FromData[TxInInfo] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val args = pair.snd
    //    new TxInInfo(
    //      fromData[TxOutRef](args.head),
    //      fromData[TxOut](args.tail.head)
    //    )

    // given FromData[TxInfo] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val args = pair.snd
    //    val fromValue = summon[FromData[Value]]
    //    new TxInfo(
    //      fromData[List[TxInInfo]](args.head),
    //      fromData[List[TxInInfo]](args.tail.head),
    //      fromData[List[TxOut]](args.tail.tail.head),
    //      fromValue(args.tail.tail.tail.head),
    //      fromValue(args.tail.tail.tail.tail.head),
    //      fromData[List[DCert]](args.tail.tail.tail.tail.tail.head),
    //      fromData[AssocMap[StakingCredential, BigInt]](args.tail.tail.tail.tail.tail.tail.head),
    //      fromData[PosixTimeRange](args.tail.tail.tail.tail.tail.tail.tail.head),
    //      fromData[List[PubKeyHash]](args.tail.tail.tail.tail.tail.tail.tail.tail.head),
    //      fromData[AssocMap[ScriptPurpose, Redeemer]](
    //        args.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
    //      ),
    //      fromData[AssocMap[DatumHash, Datum]](
    //        args.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
    //      ),
    //      fromData[TxId](args.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
    //    )

    // given FromData[ScriptContext] = (d: Data) =>
    //    val pair = unConstrData(d)
    //    val args = pair.snd
    //    new ScriptContext(
    //      fromData[TxInfo](args.head),
    //      fromData[ScriptPurpose](args.tail.head)
    //    )
}

enum OutputDatum:
    case NoOutputDatum
    case OutputDatumHash(datumHash: DatumHash)
    case OutputDatum(datum: Datum)

@Compile
object OutputDatum {
    given Eq[scalus.ledger.api.v2.OutputDatum] =
        (a: scalus.ledger.api.v2.OutputDatum, b: scalus.ledger.api.v2.OutputDatum) =>
            a match
                case NoOutputDatum =>
                    b match
                        case NoOutputDatum => true
                        case _             => false
                case OutputDatumHash(datumHash) =>
                    b match
                        case OutputDatumHash(datumHash2) => datumHash == datumHash2
                        case _                           => false
                case OutputDatum(datum) =>
                    b match
                        case OutputDatum(datum2) => datum == datum2
                        case _                   => false

    given ToData[scalus.ledger.api.v2.OutputDatum] = ToData.derived
    given FromData[scalus.ledger.api.v2.OutputDatum] = FromData.derived
}

case class TxOut(
    address: Address,
    value: Value,
    datum: OutputDatum = NoOutputDatum,
    referenceScript: Option[ScriptHash] = Option.None
)

@Compile
object TxOut {

    given ToData[TxOut] = ToData.derived

    given FromData[TxOut] = FromData.derived

}

case class TxInInfo(outRef: TxOutRef, resolved: TxOut)

@Compile
object TxInInfo {

    given ToData[TxInInfo] = ToData.derived

    given FromData[TxInInfo] = FromData.derived

}

/** A pending transaction.
  */
case class TxInfo(
    inputs: List[TxInInfo],
    referenceInputs: List[TxInInfo],
    outputs: List[TxOut],
    fee: Value,
    mint: Value,
    dcert: List[DCert],
    withdrawals: AssocMap[StakingCredential, BigInt],
    validRange: PosixTimeRange,
    signatories: List[PubKeyHash],
    redeemers: AssocMap[ScriptPurpose, Redeemer],
    data: AssocMap[DatumHash, Datum],
    id: TxId
)

@Compile
object TxInfo {
    val placeholder: TxInfo = TxInfo(
      inputs = List.empty,
      referenceInputs = List.empty,
      outputs = List.empty,
      fee = Value.zero,
      mint = Value.zero,
      dcert = List.empty,
      withdrawals = AssocMap.empty,
      validRange = Interval.always,
      signatories = List.empty,
      redeemers = AssocMap.empty,
      data = AssocMap.empty,
      id = TxId(hex"0000000000000000000000000000000000000000000000000000000000000000")
    )

    given ToData[TxInfo] = ToData.derived

    given FromData[TxInfo] = FromData.derived

}

/** The context that the currently-executing script can access.
  */
case class ScriptContext(
    txInfo: TxInfo,
    purpose: ScriptPurpose
)

@Compile
object ScriptContext {

    given ToData[ScriptContext] = ToData.derived
    given FromData[ScriptContext] = FromData.derived

}
