package scalus.ledger.api.v1

import scalus.Compile
import scalus.builtins.Builtins
import scalus.builtins.ByteString
import scalus.prelude.AssocMap
import scalus.prelude.List
import scalus.prelude.Maybe
import scalus.prelude.Prelude.===
import scalus.prelude.Prelude.given
import scalus.prelude.These.*
import scalus.uplc.Data
import scalus.uplc.Data.FromData
import scalus.uplc.Data.ToData
import scalus.uplc.Data.given
import scalus.utils.Utils.bytesToHex

type ValidatorHash = ByteString
type Datum = Data
type DatumHash = ByteString
type CurrencySymbol = ByteString
type TokenName = ByteString
type POSIXTime = BigInt
type POSIXTimeRange = Interval[POSIXTime]
type Value = AssocMap[CurrencySymbol, AssocMap[TokenName, BigInt]]

@Compile
object FromDataInstances {

  implicit def fromDataTxId(d: Data): TxId =
    val hash = summon[FromData[ByteString]].apply(d)
    new TxId(hash)

  implicit def fromDataPubKeyHash(d: Data): PubKeyHash =
    val hash = summon[FromData[ByteString]].apply(d)
    new PubKeyHash(hash)

  implicit def fromDataTxOutRef(d: Data): TxOutRef =
    val args = Builtins.unsafeDataAsConstr(d).snd
    val txidx = args.tail
    new TxOutRef(
      summon[FromData[TxId]].apply(args.head),
      summon[FromData[BigInt]].apply(txidx.head)
    )

  given FromData[DCert] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag === BigInt(0) then
      new DCert.DelegRegKey(summon[FromData[StakingCredential]].apply(args.head))
    else if tag === BigInt(1) then
      new DCert.DelegDeRegKey(summon[FromData[StakingCredential]].apply(args.head))
    else if tag === BigInt(2) then
      new DCert.DelegDelegate(
        summon[FromData[StakingCredential]].apply(args.head),
        summon[FromData[PubKeyHash]].apply(args.tail.head)
      )
    else if tag === BigInt(3) then
      new DCert.PoolRegister(
        summon[FromData[PubKeyHash]].apply(args.head),
        summon[FromData[PubKeyHash]].apply(args.tail.head)
      )
    else if tag === BigInt(4) then
      new DCert.PoolRetire(
        summon[FromData[PubKeyHash]].apply(args.head),
        summon[FromData[BigInt]].apply(args.tail.head)
      )
    else if tag === BigInt(5) then DCert.Genesis
    else if tag === BigInt(6) then DCert.Mir
    else throw new Exception(s"Unknown DCert tag: $tag")

  given ExtendedFromData[A: FromData]: FromData[Extended[A]] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag === BigInt(0) then Extended.NegInf
    else if tag === BigInt(1) then new Extended.Finite(summon[FromData[A]].apply(args.head))
    else if tag === BigInt(2) then Extended.PosInf
    else throw new Exception(s"Unknown Extended tag: $tag")

  given FromData[Credential] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag === BigInt(0) then
      new Credential.PubKeyCredential(summon[FromData[PubKeyHash]].apply(args.head))
    else if tag === BigInt(1) then
      new Credential.ScriptCredential(summon[FromData[ByteString]].apply(args.head))
    else throw new Exception(s"Unknown Credential tag: $tag")

  given FromData[StakingCredential] =
    (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      val tag = pair.fst
      if tag === BigInt(0) then
        new StakingCredential.StakingHash(summon[FromData[Credential]].apply(pair.snd.head))
      else if tag === BigInt(1) then
        val fromBI = summon[FromData[BigInt]]
        val ptrs = pair.snd
        new StakingCredential.StakingPtr(
          fromBI.apply(pair.snd.head),
          fromBI.apply(pair.snd.tail.head),
          fromBI.apply(pair.snd.tail.tail.head)
        )
      else throw new RuntimeException("Invalid tag")

  given FromData[ScriptPurpose] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag === BigInt(0) then
      new ScriptPurpose.Minting(summon[FromData[TokenName]].apply(args.head))
    else if tag === BigInt(1) then
      new ScriptPurpose.Spending(summon[FromData[TxOutRef]].apply(args.head))
    else if tag === BigInt(2) then
      new ScriptPurpose.Rewarding(summon[FromData[StakingCredential]].apply(args.head))
    else if tag === BigInt(3) then
      new ScriptPurpose.Certifying(summon[FromData[DCert]].apply(args.head))
    else throw new Exception(s"Unknown ScriptPurpose tag: $tag")

  given FromData[Address] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    new Address(
      summon[FromData[Credential]].apply(pair.snd.head),
      summon[FromData[Maybe[StakingCredential]]].apply(pair.snd.tail.head)
    )

  given FromData[TxOut] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new TxOut(
      summon[FromData[Address]].apply(args.head),
      summon[FromData[Value]].apply(args.tail.head),
      summon[FromData[Maybe[DatumHash]]].apply(args.tail.tail.head)
    )

  given FromData[TxInInfo] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new TxInInfo(
      summon[FromData[TxOutRef]].apply(args.head),
      summon[FromData[TxOut]].apply(args.tail.head)
    )

  given UpperBoundFromData[A: FromData]: FromData[UpperBound[A]] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new UpperBound(
      summon[FromData[Extended[A]]].apply(args.head),
      summon[FromData[Closure]].apply(args.tail.head)
    )

  given LowerBoundFromData[A: FromData]: FromData[LowerBound[A]] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new LowerBound(
      summon[FromData[Extended[A]]].apply(args.head),
      summon[FromData[Closure]].apply(args.tail.head)
    )

  given IntervalFromData[A: FromData]: FromData[Interval[A]] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new Interval(
      summon[FromData[LowerBound[A]]].apply(args.head),
      summon[FromData[UpperBound[A]]].apply(args.tail.head)
    )

    /*
     txInfoInputs: List[TxInInfo],
    txInfoOutputs: List[TxOut],
    txInfoFee: Value,
    txInfoMint: Value,
    txInfoDCert: List[DCert],
    txInfoWdrl: List[(StakingCredential, BigInt)],
    txInfoValidRange: POSIXTimeRange,
    txInfoSignatories: List[PubKeyHash],
    txInfoData: List[(DatumHash, Datum)],
    txInfoId: TxId */
  given FromData[TxInfo] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    val fromValue = summon[FromData[Value]]
    val txInfoWdrl = summon[FromData[List[(StakingCredential, BigInt)]]].apply(
      args.tail.tail.tail.tail.tail.head
    )
    // new TxInfo(null, null, null, null, null, null, null, null, null, null)
    new TxInfo(
      summon[FromData[List[TxInInfo]]].apply(args.head),
      summon[FromData[List[TxOut]]].apply(args.tail.head),
      fromValue.apply(args.tail.tail.head),
      fromValue.apply(args.tail.tail.tail.head),
      summon[FromData[List[DCert]]].apply(args.tail.tail.tail.tail.head),
      txInfoWdrl,
      summon[FromData[Interval[POSIXTime]]].apply(
        args.tail.tail.tail.tail.tail.tail.head
      ),
      summon[FromData[List[PubKeyHash]]].apply(
        args.tail.tail.tail.tail.tail.tail.tail.head
      ),
      summon[FromData[List[(DatumHash, Datum)]]].apply(
        args.tail.tail.tail.tail.tail.tail.tail.tail.head
      ),
      summon[FromData[TxId]].apply(args.tail.tail.tail.tail.tail.tail.tail.tail.tail.head)
    )

  given FromData[ScriptContext] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val args = pair.snd
    new ScriptContext(
      summon[FromData[TxInfo]].apply(args.head),
      summon[FromData[ScriptPurpose]].apply(args.tail.head)
    )

}

type Closure = Boolean
enum Extended[+A]:
  case NegInf extends Extended[Nothing]
  case Finite(a: A)
  case PosInf extends Extended[Nothing]

import ToDataInstances.given

case class UpperBound[A](upper: Extended[A], closure: Closure) derives ToData
case class LowerBound[A](extended: Extended[A], closure: Closure) derives ToData
case class Interval[A](from: LowerBound[A], to: UpperBound[A]) derives ToData

@Compile
object Interval:
  def always[A]: Interval[A] =
    new Interval(new LowerBound(Extended.NegInf, true), new UpperBound(Extended.PosInf, true))

enum DCert:
  case DelegRegKey(cred: StakingCredential)
  case DelegDeRegKey(cred: StakingCredential)
  case DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash)
  case PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash)
  case PoolRetire(poolId: PubKeyHash, epoch: BigInt)
  case Genesis
  case Mir

case class TxId(hash: ByteString):
  override def toString = s"TxId(${hash.toHex})"

/*
data TxOutRef = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
 */
case class TxOutRef(txOutRefId: TxId, txOutRefIdx: BigInt) derives Data.ToData

case class PubKeyHash(hash: ByteString) {
  override def toString = s"PubKeyHash(${hash})"
}

enum Credential:
  case PubKeyCredential(hash: PubKeyHash)
  case ScriptCredential(hash: ValidatorHash)

//data StakingCredential
//    = StakingHash Credential
//    | StakingPtr Integer Integer Integer
enum StakingCredential:
  case StakingHash(cred: Credential)
  case StakingPtr(a: BigInt, b: BigInt, c: BigInt)

case class Address(
    addressCredential: Credential,
    addressStakingCredential: Maybe[StakingCredential]
) derives Data.ToData
case class TxOut(txOutAddress: Address, txOutValue: Value, txOutDatumHash: Maybe[DatumHash])
    derives Data.ToData

// TxInInfo
case class TxInInfo(
    txInInfoOutRef: TxOutRef,
    txInInfoResolved: TxOut
) derives Data.ToData

/*
data TxInfo = TxInfo
    { txInfoInputs      :: [TxInInfo] -- ^ Transaction inputs
    , txInfoOutputs     :: [TxOut] -- ^ Transaction outputs
    , txInfoFee         :: Value -- ^ The fee paid by this transaction.
    , txInfoMint        :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert       :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl        :: [(StakingCredential, Integer)] -- ^ Withdrawals
    , txInfoValidRange  :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoData        :: [(DatumHash, Datum)]
    , txInfoId          :: TxId
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving stock (Generic, Haskell.Show, Haskell.Eq)
 */
case class TxInfo(
    txInfoInputs: List[TxInInfo],
    txInfoOutputs: List[TxOut],
    txInfoFee: Value,
    txInfoMint: Value,
    txInfoDCert: List[DCert],
    txInfoWdrl: List[(StakingCredential, BigInt)],
    txInfoValidRange: POSIXTimeRange,
    txInfoSignatories: List[PubKeyHash],
    txInfoData: List[(DatumHash, Datum)],
    txInfoId: TxId
) derives Data.ToData

enum ScriptPurpose:
  case Minting(curSymbol: ByteString)
  case Spending(txOutRef: TxOutRef)
  case Rewarding(stakingCred: StakingCredential)
  case Certifying(cert: DCert)

// data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
case class ScriptContext(scriptContextTxInfo: TxInfo, scriptContextPurpose: ScriptPurpose)
    derives Data.ToData
