package scalus.ledger.api.v1

import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Data
import scalus.uplc.Data.Lift
import scalus.utils.Utils.bytesToHex

type ValidatorHash = Array[Byte]
type Datum = Data
type DatumHash = Array[Byte]
type CurrencySymbol = Array[Byte]
type TokenName = Array[Byte]
type POSIXTime = BigInt
type POSIXTimeRange = Interval[POSIXTime]
opaque type AssocMap[K, V] = List[(K, V)]
opaque type Value = AssocMap[CurrencySymbol, AssocMap[TokenName, BigInt]]
object Value:
  val zero: Value = List.empty
  def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value = List((cs, List((tn, v))))
  def lovelace(v: BigInt): Value = apply(Array.empty, Array.empty, v)
  def asLists(v: Value): List[(CurrencySymbol, List[(TokenName, BigInt)])] = v

object Instances:
  import scalus.uplc.Data.toData

  given Lift[TxId] with
    def lift(a: TxId): Data = a.id.toData

  given Lift[Value] with
    def lift(a: Value): Data = Value.asLists(a).toData

  given Lift[PubKeyHash] with
    def lift(a: PubKeyHash): Data = a.hash.toData

  given DCertLift[T <: DCert]: Lift[T] with
    def lift(a: T): Data =
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

  given ExtendedLift[A: Lift, T[A] <: Extended[A]]: Lift[T[A]] with
    def lift(a: T[A]): Data =
      a match
        case Extended.NegInf    => Data.Constr(0, Nil)
        case Extended.Finite(a) => Data.Constr(1, a.toData :: Nil)
        case Extended.PosInf    => Data.Constr(2, Nil)
  given CredentialLift[T <: Credential]: Lift[T] with
    def lift(a: T): Data =
      a match
        case a: Credential.PubKeyCredential =>
          Lift.deriveProduct[Credential.PubKeyCredential](0).lift(a)
        case a: Credential.ScriptCredential =>
          Lift.deriveProduct[Credential.ScriptCredential](1).lift(a)

  given StakingCredentialLift[T <: StakingCredential]: Lift[T] with
    def lift(a: T): Data =
      a match
        case a: StakingCredential.StakingHash =>
          Lift.deriveProduct[StakingCredential.StakingHash](0).lift(a)
        case a: StakingCredential.StakingPtr =>
          Lift.deriveProduct[StakingCredential.StakingPtr](1).lift(a)
  given ScriptPurposeLift[T <: ScriptPurpose]: Lift[T] with
    def lift(a: T): Data =
      a match
        case a: ScriptPurpose.Minting    => Lift.deriveProduct[ScriptPurpose.Minting](0).lift(a)
        case a: ScriptPurpose.Spending   => Lift.deriveProduct[ScriptPurpose.Spending](1).lift(a)
        case a: ScriptPurpose.Rewarding  => Lift.deriveProduct[ScriptPurpose.Rewarding](2).lift(a)
        case a: ScriptPurpose.Certifying => Lift.deriveProduct[ScriptPurpose.Certifying](3).lift(a)
end Instances

type Closure = Boolean
enum Extended[+A]:
  case NegInf extends Extended[Nothing]
  case Finite(a: A)
  case PosInf extends Extended[Nothing]

case class UpperBound[A](upper: Extended[A], closure: Closure) derives Lift
case class LowerBound[A](extended: Extended[A], closure: Closure) derives Lift
case class Interval[A](from: LowerBound[A], to: UpperBound[A]) derives Lift
object Interval:
  def always[A]: Interval[A] =
    Interval(LowerBound(Extended.NegInf, true), UpperBound(Extended.PosInf, true))

// data DCert
//  = DCertDelegRegKey StakingCredential
//  | DCertDelegDeRegKey StakingCredential
//  | DCertDelegDelegate
//      StakingCredential
//      -- ^ delegator
//      PubKeyHash
//      -- ^ delegatee
//  | -- | A digest of the PoolParams
//    DCertPoolRegister
//      PubKeyHash
//      -- ^ poolId
//      PubKeyHash
//      -- ^ pool VFR
//  | -- | The retiremant certificate and the Epoch N
//    DCertPoolRetire PubKeyHash Integer -- NB: Should be Word64 but we only have Integer on-chain
//  | -- | A really terse Digest
//    DCertGenesis
//  | -- | Another really terse Digest
//    DCertMir
enum DCert:
  case DelegRegKey(cred: StakingCredential)
  case DelegDeRegKey(cred: StakingCredential)
  case DelegDelegate(cred: StakingCredential, delegatee: PubKeyHash)
  case PoolRegister(poolId: PubKeyHash, vrf: PubKeyHash)
  case PoolRetire(poolId: PubKeyHash, epoch: BigInt)
  case Genesis
  case Mir

case class TxId(id: Array[Byte]) {
  override def toString = s"TxId(${bytesToHex(id)})"
}
/*
data TxOutRef = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
 */
case class TxOutRef(txOutRefId: TxId, txOutRefIdx: BigInt) derives Data.Lift

case class PubKeyHash(hash: Array[Byte]) {
  override def toString = s"PubKeyHash(${bytesToHex(hash)})"
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
    addressStakingCredential: Option[StakingCredential]
) derives Data.Lift
case class TxOut(txOutAddress: Address, txOutValue: Value, txOutDatumHash: Option[DatumHash])
    derives Data.Lift

// TxInInfo
case class TxInInfo(
    txInInfoOutRef: TxOutRef,
    txInInfoResolved: TxOut
) derives Data.Lift

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
) derives Data.Lift

/*
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
 */
enum ScriptPurpose:
  case Minting(curSymbol: Array[Byte])
  case Spending(txOutRef: TxOutRef)
  case Rewarding(stakingCred: StakingCredential)
  case Certifying(cert: DCert)

// data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
case class ScriptContext(scriptContextTxInfo: TxInfo, scriptContextPurpose: ScriptPurpose)
    derives Data.Lift
