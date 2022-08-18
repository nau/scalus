package scalus.ledger.api.v1

import scalus.ledger.api.v1.Instances.given
import scalus.uplc.Data
import scalus.uplc.Data.Lift
import scalus.utils.Utils.bytesToHex

object Instances:
  given ScriptPurposeLift[T <: ScriptPurpose]: Lift[T] with
    def lift(a: T): Data =
      a match
        case a: ScriptPurpose.Minting  => Lift.deriveProduct[ScriptPurpose.Minting](0).lift(a)
        case a: ScriptPurpose.Spending => Lift.deriveProduct[ScriptPurpose.Spending](1).lift(a)

case class TxId(id: Array[Byte]) derives Data.Lift {
  override def toString = s"TxId(${bytesToHex(id)})"
}
/*
data TxOutRef = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
 */
case class TxOutRef(txOutRefId: TxId, txOutRefIdx: Int) derives Data.Lift

case class PubKeyHash(hash: Array[Byte]) derives Data.Lift {
  override def toString = s"PubKeyHash(${bytesToHex(hash)})"
}

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
    txInfoInputs: List[Int],
    txInfoOutputs: List[Int],
    txInfoFee: Int,
    txInfoMint: Int,
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
//    case Rewarding(StakingCredential)
//    case Certifying(DCert)

// data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
case class ScriptContext(scriptContextTxInfo: TxInfo, scriptContextPurpose: ScriptPurpose)
    derives Data.Lift
