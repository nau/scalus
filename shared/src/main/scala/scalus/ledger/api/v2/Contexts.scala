package scalus.ledger.api.v2

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.fromData
import scalus.prelude.AssocMap
import scalus.prelude.List
import scalus.prelude.Maybe
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.Eq
import scalus.prelude.Prelude.given
import scalus.prelude.These.*

@Compile
object FromDataInstances {
    import scalus.builtin.FromDataInstances.given
    import scalus.ledger.api.v1.FromDataInstances.given

    given FromData[OutputDatum] = (d: Data) =>
        val pair = Builtins.unsafeDataAsConstr(d)
        val tag = pair.fst
        val args = pair.snd
        if tag === BigInt(0) then OutputDatum.NoOutputDatum
        else if tag === BigInt(1) then
            new OutputDatum.OutputDatumHash(fromData[DatumHash](args.head))
        else if tag === BigInt(2) then new OutputDatum.OutputDatum(fromData[Datum](args.head))
        else throw new Exception("PT1")

    given FromData[TxOut] = (d: Data) =>
        val pair = Builtins.unsafeDataAsConstr(d)
        val args = pair.snd
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
                        case OutputDatumHash(datumHash2) => datumHash === datumHash2
                        case _                           => false
                case OutputDatum(datum) =>
                    b match
                        case OutputDatum(datum2) => datum === datum2
                        case _                   => false
}

case class TxOut(
    address: Address,
    value: Value,
    datum: OutputDatum,
    referenceScript: Maybe[ScriptHash]
)

case class TxInInfo(outRef: TxOutRef, resolved: TxOut)

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
    validRange: POSIXTimeRange,
    signatories: List[PubKeyHash],
    redeemers: AssocMap[ScriptPurpose, Redeemer],
    data: AssocMap[DatumHash, Datum],
    id: TxId
)

/** The context that the currently-executing script can access.
  */
case class ScriptContext(
    txInfo: TxInfo,
    purpose: ScriptPurpose
)
