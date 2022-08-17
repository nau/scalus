package scalus.ledger.api.v2

import scalus.utils.Utils.bytesToHex

case class TxId(id: Array[Byte]) {
  override def toString = s"TxId(${bytesToHex(id)})"
}
/*
data TxOutRef = TxOutRef {
    txOutRefId  :: TxId,
    txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
    }
 */
case class TxOutRef(txOutRefId: TxId, txOutRefIdx: Int)

case class TxInfo(
    txInfoInputs: List[Int]
//, txInfoReferenceInputs : [TxInInfo] -- ^ Transaction reference inputs
//, txInfoOutputs         : [TxOut] -- ^ Transaction outputs
//, txInfoFee             : Value -- ^ The fee paid by this transaction.
//, txInfoMint            : Value -- ^ The 'Value' minted by this transaction.
//, txInfoDCert           : [DCert] -- ^ Digests of certificates included in this transaction
//, txInfoWdrl            : Map StakingCredential Integer -- ^ Withdrawals
//, txInfoValidRange      : POSIXTimeRange -- ^ The valid range for the transaction.
//, txInfoSignatories     : [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
//, txInfoRedeemers       : Map ScriptPurpose Redeemer
//, txInfoData            : Map DatumHash Datum
//, txInfoId              : TxId)
)

/*
data ScriptPurpose
    = Minting CurrencySymbol
    | Spending TxOutRef
    | Rewarding StakingCredential
    | Certifying DCert
 */
enum ScriptPurpose {
//    case Minting(CurrencySymbol)
  case Spending(txOutRef: TxOutRef)
//    case Rewarding(StakingCredential)
//    case Certifying(DCert)
}

// data ScriptContext = ScriptContext{scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose }
case class ScriptContext(scriptContextTxInfo: TxInfo, scriptContextPurpose: ScriptPurpose)
