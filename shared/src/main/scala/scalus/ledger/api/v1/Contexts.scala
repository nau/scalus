package scalus.ledger.api.v1

import scalus.builtins.ByteString
import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Data
import scalus.uplc.Data.{FromData, ToData}
import scalus.utils.Utils.bytesToHex
import scalus.Predef.{List, Maybe}
import scalus.builtins.Builtins

type ValidatorHash = ByteString
type Datum = Data
type DatumHash = ByteString
type CurrencySymbol = ByteString
type TokenName = ByteString
type POSIXTime = BigInt
type POSIXTimeRange = Interval[POSIXTime]
type AssocMap[K, V] = List[(K, V)]
type Value = AssocMap[CurrencySymbol, AssocMap[TokenName, BigInt]]
object Value:
  val zero: Value = List.empty
  def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value = List((cs, List((tn, v))))
  def lovelace(v: BigInt): Value = apply(ByteString.empty, ByteString.empty, v)
  def asLists(v: Value): List[(CurrencySymbol, List[(TokenName, BigInt)])] = v

object Instances:
  import scalus.uplc.Data.toData

  given ToData[TxId] with
    def toData(a: TxId): Data = a.hash.toData

  given FromData[TxId] = (d: Data) =>
    val hash = summon[FromData[ByteString]].apply(d)
    new TxId(hash)

  given ToData[PubKeyHash] with
    def toData(a: PubKeyHash): Data = a.hash.toData

  given FromData[PubKeyHash] = (d: Data) =>
    val hash = summon[FromData[ByteString]].apply(d)
    new PubKeyHash(hash)

  given FromData[TxOutRef] = (d: Data) =>
    val args = Builtins.unsafeDataAsConstr(d).snd
    new TxOutRef(
      summon[FromData[TxId]].apply(args.head),
      summon[FromData[BigInt]].apply(args.tail.head)
    )

  given DCertLift[T <: DCert]: ToData[T] with
    def toData(a: T): Data =
      a match
        case DCert.DelegRegKey(cred: StakingCredential)   => Data.Constr(0, cred.toData :: Nil)
        case DCert.DelegDeRegKey(cred: StakingCredential) => Data.Constr(1, cred.toData :: Nil)
        case DCert.DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash) =>
          Data.Constr(2, cred.toData :: delegatee.toData :: Nil)
        case DCert.PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash) =>
          Data.Constr(3, poolId.toData :: vrf.toData :: Nil)
        case DCert.PoolRetire(poolId: PubKeyHash, epoch: BigInt) =>
          Data.Constr(4, poolId.toData :: epoch.toData :: Nil)
        case DCert.Genesis => Data.Constr(5, Nil)
        case DCert.Mir     => Data.Constr(6, Nil)

  given FromData[DCert] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag == BigInt(0) then DCert.DelegRegKey(summon[FromData[StakingCredential]].apply(args.head))
    else if tag == BigInt(1) then
      DCert.DelegDeRegKey(summon[FromData[StakingCredential]].apply(args.head))
    else if tag == BigInt(2) then
      DCert.DelegDelegate(
        summon[FromData[StakingCredential]].apply(args.head),
        summon[FromData[PubKeyHash]].apply(args.tail.head)
      )
    else if tag == BigInt(3) then
      DCert.PoolRegister(
        summon[FromData[PubKeyHash]].apply(args.head),
        summon[FromData[PubKeyHash]].apply(args.tail.head)
      )
    else if tag == BigInt(4) then
      DCert.PoolRetire(
        summon[FromData[PubKeyHash]].apply(args.head),
        summon[FromData[BigInt]].apply(args.tail.head)
      )
    else if tag == BigInt(5) then DCert.Genesis
    else if tag == BigInt(6) then DCert.Mir
    else throw new Exception(s"Unknown DCert tag: $tag")

  given ExtendedLift[A: ToData, T[A] <: Extended[A]]: ToData[T[A]] with
    def toData(a: T[A]): Data =
      a match
        case Extended.NegInf    => Data.Constr(0, Nil)
        case Extended.Finite(a) => Data.Constr(1, a.toData :: Nil)
        case Extended.PosInf    => Data.Constr(2, Nil)

  given ExtendedFromData[A: FromData]: FromData[Extended[A]] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag == BigInt(0) then Extended.NegInf
    else if tag == BigInt(1) then Extended.Finite(summon[FromData[A]].apply(args.head))
    else if tag == BigInt(2) then Extended.PosInf
    else throw new Exception(s"Unknown Extended tag: $tag")

  given CredentialToData[T <: Credential]: ToData[T] with
    def toData(a: T): Data =
      a match
        case a: Credential.PubKeyCredential =>
          ToData.deriveProduct[Credential.PubKeyCredential](0).toData(a)
        case a: Credential.ScriptCredential =>
          ToData.deriveProduct[Credential.ScriptCredential](1).toData(a)

  given FromData[Credential] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag == BigInt(0) then
      new Credential.PubKeyCredential(summon[FromData[PubKeyHash]].apply(args.head))
    else if tag == BigInt(1) then
      new Credential.ScriptCredential(summon[FromData[ByteString]].apply(args.head))
    else throw new Exception(s"Unknown Credential tag: $tag")

  given StakingCredentialLift[T <: StakingCredential]: ToData[T] with
    def toData(a: T): Data =
      a match
        case a: StakingCredential.StakingHash =>
          ToData.deriveProduct[StakingCredential.StakingHash](0).toData(a)
        case a: StakingCredential.StakingPtr =>
          ToData.deriveProduct[StakingCredential.StakingPtr](1).toData(a)

  given FromData[StakingCredential] =
    (d: Data) =>
      val pair = Builtins.unsafeDataAsConstr(d)
      val tag = pair.fst
      if tag == BigInt(0) then
        new StakingCredential.StakingHash(summon[FromData[Credential]].apply(pair.snd.head))
      else if tag == BigInt(1) then
        val fromBI = summon[FromData[BigInt]]
        val ptrs = pair.snd
        new StakingCredential.StakingPtr(
          fromBI.apply(pair.snd.head),
          fromBI.apply(pair.snd.tail.head),
          fromBI.apply(pair.snd.tail.tail.head)
        )
      else throw new RuntimeException("Invalid tag")
  given ScriptPurposeLift[T <: ScriptPurpose]: ToData[T] with
    def toData(a: T): Data =
      a match
        case a: ScriptPurpose.Minting  => ToData.deriveProduct[ScriptPurpose.Minting](0).toData(a)
        case a: ScriptPurpose.Spending => ToData.deriveProduct[ScriptPurpose.Spending](1).toData(a)
        case a: ScriptPurpose.Rewarding =>
          ToData.deriveProduct[ScriptPurpose.Rewarding](2).toData(a)
        case a: ScriptPurpose.Certifying =>
          ToData.deriveProduct[ScriptPurpose.Certifying](3).toData(a)

  given FromData[ScriptPurpose] = (d: Data) =>
    val pair = Builtins.unsafeDataAsConstr(d)
    val tag = pair.fst
    val args = pair.snd
    if tag == BigInt(0) then new ScriptPurpose.Minting(summon[FromData[TokenName]].apply(args.head))
    else if tag == BigInt(1) then
      new ScriptPurpose.Spending(summon[FromData[TxOutRef]].apply(args.head))
    else if tag == BigInt(2) then
      new ScriptPurpose.Rewarding(summon[FromData[StakingCredential]].apply(args.head))
    else if tag == BigInt(3) then
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

  // given UpperBoundToData[A: ToData]: ToData[UpperBound[A]] = (a: UpperBound[A]) =>
    // Builtins.mkConstr(0, scalus.builtins.List(a.upper.toData, a.closure.toData))

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
end Instances

type Closure = Boolean
enum Extended[+A]:
  case NegInf extends Extended[Nothing]
  case Finite(a: A)
  case PosInf extends Extended[Nothing]

case class UpperBound[A](upper: Extended[A], closure: Closure) derives ToData
case class LowerBound[A](extended: Extended[A], closure: Closure) derives ToData
case class Interval[A](from: LowerBound[A], to: UpperBound[A]) derives ToData
object Interval:
  def always[A]: Interval[A] =
    Interval(LowerBound(Extended.NegInf, true), UpperBound(Extended.PosInf, true))

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
