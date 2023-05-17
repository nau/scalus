package scalus.ledger.api.v2

import scalus.Compile
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.prelude.AssocMap
import scalus.prelude.List
import scalus.prelude.Maybe
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.prelude.These.*
import scalus.builtins
import scalus.uplc.Data
import scalus.uplc.Data.fromData
import scalus.uplc.Data.FromData
import scalus.uplc.Data.ToData
import scalus.uplc.Data.given
import scalus.uplc.DataInstances.given
import scalus.utils.Utils.bytesToHex
import scalus.ledger.api.v1.TxOutRef
import scalus.ledger.api.v1.Value
import scalus.ledger.api.v1.DCert
import scalus.ledger.api.v1.StakingCredential
import scalus.ledger.api.v1.POSIXTimeRange
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v1.ScriptPurpose
import scalus.ledger.api.v1.Redeemer
import scalus.ledger.api.v1.Datum
import scalus.ledger.api.v1.TxId
import scalus.ledger.api.v1.DatumHash
import scalus.ledger.api.v1.ToDataInstances.given
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.ScriptHash
import scalus.ledger.api.v1.FromDataInstances.given

@Compile
object FromDataInstances {

  given FromData[OutputDatum] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag === BigInt(0) then OutputDatum.NoOutputDatum
    else if tag === BigInt(1) then new OutputDatum.OutputDatumHash(fromData[DatumHash](args.head))
    else if tag === BigInt(2) then new OutputDatum.OutputDatum(fromData[Datum](args.head))
    else throw new Exception("PT1")

  given FromData[TxOut] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    /* txOutAddress: Address,
    txOutValue: Value,
    txOutDatum: OutputDatum,
    txOutReferenceScript: Maybe[ScriptHash] */
    new TxOut(
      fromData[Address](args.head),
      fromData[Value](args.tail.head),
      fromData[OutputDatum](args.tail.tail.head),
      fromData[Maybe[ScriptHash]](args.tail.tail.tail.head)
    )

  given FromData[TxInInfo] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new TxInInfo(
      fromData[TxOutRef](args.head),
      fromData[TxOut](args.tail.head)
    )

  given FromData[TxInfo] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    val fromValue = summon[FromData[Value]]
    /* txInfoInputs: List[TxInInfo], 1
    txInfoReferenceInputs: List[TxInInfo],
    txInfoOutputs: List[TxOut], 3
    txInfoFee: Value,
    txInfoMint: Value, 5
    txInfoDCert: List[DCert],
    txInfoWdrl: AssocMap[StakingCredential, BigInt], 7
    txInfoValidRange: POSIXTimeRange,
    txInfoSignatories: List[PubKeyHash], 9
    txInfoRedeemers: AssocMap[ScriptPurpose, Redeemer],
    txInfoData: AssocMap[DatumHash, Datum], 11
    txInfoId: TxId */
    new TxInfo(
      fromData[List[TxInInfo]](args.head),
      fromData[List[TxInInfo]](args.tail.head),
      fromData[List[TxOut]](args.tail.tail.head),
      fromValue(args.tail.tail.tail.head),
      fromValue(args.tail.tail.tail.tail.head),
      fromData[List[DCert]](args.tail.tail.tail.tail.tail.head),
      fromData[AssocMap[StakingCredential, BigInt]](args.tail.tail.tail.tail.tail.tail.head),
      fromData[POSIXTimeRange](args.tail.tail.tail.tail.tail.tail.tail.head),
      fromData[List[PubKeyHash]](args.tail.tail.tail.tail.tail.tail.tail.tail.head),
      fromData[AssocMap[ScriptPurpose, Redeemer]](
        args.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
      ),
      fromData[AssocMap[DatumHash, Datum]](
        args.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head
      ),
      fromData[TxId](args.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
    )

  given FromData[ScriptContext] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new ScriptContext(
      fromData[TxInfo](args.head),
      fromData[ScriptPurpose](args.tail.head)
    )
}

object ToDataInstances {
  import scalus.uplc.Data.toData

  given OutputDatumToData[T <: OutputDatum]: ToData[T] with
    def toData(d: T): Data =
      d match
        case OutputDatum.NoOutputDatum => Builtins.mkConstr(0, Builtins.mkNilData)
        case OutputDatum.OutputDatumHash(datumHash) =>
          Builtins.mkConstr(1, builtins.List(datumHash.toData))
        case OutputDatum.OutputDatum(datum) => Builtins.mkConstr(2, builtins.List(datum))
}

import ToDataInstances.given

enum OutputDatum:
  case NoOutputDatum
  case OutputDatumHash(datumHash: DatumHash)
  case OutputDatum(datum: Datum)

case class TxOut(
    txOutAddress: Address,
    txOutValue: Value,
    txOutDatum: OutputDatum,
    txOutReferenceScript: Maybe[ScriptHash]
) derives ToData

case class TxInInfo(txInInfoOutRef: TxOutRef, txInInfoResolved: TxOut) derives ToData

/** A pending transaction.
  */
case class TxInfo(
    txInfoInputs: List[TxInInfo],
    txInfoReferenceInputs: List[TxInInfo],
    txInfoOutputs: List[TxOut],
    txInfoFee: Value,
    txInfoMint: Value,
    txInfoDCert: List[DCert],
    txInfoWdrl: AssocMap[StakingCredential, BigInt],
    txInfoValidRange: POSIXTimeRange,
    txInfoSignatories: List[PubKeyHash],
    txInfoRedeemers: AssocMap[ScriptPurpose, Redeemer],
    txInfoData: AssocMap[DatumHash, Datum],
    txInfoId: TxId
) derives ToData

/** The context that the currently-executing script can access.
  */
case class ScriptContext(
    scriptContextTxInfo: TxInfo,
    scriptContextPurpose: ScriptPurpose
) derives ToData
