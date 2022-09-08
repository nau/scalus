package scalus.ledger.api.v1

import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Data
import scalus.uplc.Data.Lift
import scalus.utils.Utils.bytesToHex

type ValidatorHash = Array[Byte]
type DatumHash = Array[Byte]
type CurrencySymbol = Array[Byte]
type TokenName = Array[Byte]
opaque type AssocMap[K, V] = List[(K, V)]
opaque type Value = AssocMap[CurrencySymbol, AssocMap[TokenName, BigInt]]
object Value:
  val zero: Value = List.empty
  def apply(cs: CurrencySymbol, tn: TokenName, v: BigInt): Value = List((cs, List((tn, v))))
  def lovelace(v: BigInt): Value = apply(Array.empty, Array.empty, v)
  def asLists(v: Value): List[(CurrencySymbol, List[(TokenName, BigInt)])] = v

object Instances:
  import scalus.uplc.Data.toData

  given OptionLift[A: Lift]: Lift[Option[A]] with
    def lift(a: Option[A]): Data = a match
      case Some(v) => Data.Constr(0, List(v.toData))
      case None    => Data.Constr(1, List.empty)

  given Lift[TxId] with
    def lift(a: TxId): Data = a.id.toData

  given Lift[Value] with
    def lift(a: Value): Data = Value.asLists(a).toData

  given Lift[PubKeyHash] with
    def lift(a: PubKeyHash): Data = a.hash.toData

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
        case a: ScriptPurpose.Minting   => Lift.deriveProduct[ScriptPurpose.Minting](0).lift(a)
        case a: ScriptPurpose.Spending  => Lift.deriveProduct[ScriptPurpose.Spending](1).lift(a)
        case a: ScriptPurpose.Rewarding => Lift.deriveProduct[ScriptPurpose.Rewarding](2).lift(a)

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
    txInfoDCert: List[Int],
    txInfoWdrl: List[(Int, Int)],
    txInfoValidRange: Int,
    txInfoSignatories: List[PubKeyHash],
    txInfoData: List[(Int, Int)],
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
//    case Certifying(DCert)

// data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
case class ScriptContext(scriptContextTxInfo: TxInfo, scriptContextPurpose: ScriptPurpose)
    derives Data.Lift
