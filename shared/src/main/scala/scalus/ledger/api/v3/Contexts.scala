package scalus.ledger.api.v3

import scalus.Compile
import scalus.builtin
import scalus.builtin.Builtins.*
import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.builtin.FromData
import scalus.builtin.ToData
import scalus.ledger.api.v1.*
import scalus.ledger.api.v2
import scalus.prelude.*

export scalus.ledger.api.v1.Address
export scalus.ledger.api.v1.Closure
export scalus.ledger.api.v1.Credential
export scalus.ledger.api.v1.CurrencySymbol
export scalus.ledger.api.v1.DCert
export scalus.ledger.api.v1.Datum
export scalus.ledger.api.v1.DatumHash
export scalus.ledger.api.v1.IntervalBoundType
export scalus.ledger.api.v1.FromDataInstances.given
export scalus.ledger.api.v1.Interval
export scalus.ledger.api.v1.IntervalBound
export scalus.ledger.api.v1.PosixTime
export scalus.ledger.api.v1.PosixTimeRange
export scalus.ledger.api.v1.PubKeyHash
export scalus.ledger.api.v1.Redeemer
export scalus.ledger.api.v1.RedeemerHash
export scalus.ledger.api.v1.ScriptHash
export scalus.ledger.api.v1.StakingCredential
export scalus.ledger.api.v1.ToDataInstances.given
export scalus.ledger.api.v1.TokenName
export scalus.ledger.api.v1.ValidatorHash
export scalus.ledger.api.v1.Value
export scalus.ledger.api.v2.TxOut
export scalus.prelude.Prelude.*

@Compile
object FromDataInstances {
    import scalus.builtin.FromDataInstances.given
    import scalus.ledger.api.v1.FromDataInstances.given
    import scalus.ledger.api.v2.FromDataInstances.given

    given FromData[TxId] = (d: Data) => TxId(d.toByteString)
    given FromData[TxOutRef] = FromData.deriveCaseClass
    given FromData[DRep] = FromData.deriveEnum
    given FromData[Delegatee] = FromData.deriveEnum
    given FromData[TxCert] = FromData.deriveEnum
    given FromData[Voter] = FromData.deriveEnum
    given FromData[Vote] = FromData.deriveEnum
    given FromData[GovernanceActionId] = FromData.deriveCaseClass
    given FromData[Committee] = FromData.deriveCaseClass
    given FromData[ProtocolVersion] = FromData.deriveCaseClass
    given FromData[GovernanceAction] = FromData.deriveEnum
    given FromData[ProposalProcedure] = FromData.deriveCaseClass
    given FromData[ScriptPurpose] = FromData.deriveEnum
    given FromData[ScriptInfo] = FromData.deriveEnum
    given FromData[TxInInfo] = FromData.deriveCaseClass
    given FromData[TxInfo] = FromData.deriveCaseClass
    given FromData[ScriptContext] = FromData.deriveCaseClass

    @deprecated("Use ScriptInfo instead")
    given FromData[SpendingScriptInfo] = (d: Data) =>
        val pair = d.toConstr
        if pair.fst == BigInt(1) then
            val args = pair.snd
            SpendingScriptInfo(args.head.to[TxOutRef], args.tail.head.to[Option[Datum]])
        else throw new Exception("Invalid SpendingScriptInfo")

    @deprecated("Use ScriptContext instead")
    given FromData[SpendingScriptContext] = FromData.deriveCaseClass
}

@Compile
object ToDataInstances {
    import scalus.builtin.ToDataInstances.given
    import scalus.ledger.api.v1.ToDataInstances.given
    import scalus.ledger.api.v2.ToDataInstances.given

    given ToData[TxId] = (x: TxId) => bData(x.hash)
    given ToData[TxOutRef] = ToData.deriveCaseClass[TxOutRef](0)
    given ToData[DRep] = ToData.deriveEnum
    given ToData[Delegatee] = ToData.deriveEnum
    given ToData[TxCert] = ToData.deriveEnum
    given ToData[Voter] = ToData.deriveEnum
    given ToData[Vote] = ToData.deriveEnum
    given ToData[GovernanceActionId] = ToData.deriveCaseClass[GovernanceActionId](0)
    given ToData[Committee] = ToData.deriveCaseClass[Committee](0)
    given ToData[ProtocolVersion] = ToData.deriveCaseClass[ProtocolVersion](0)
    given ToData[GovernanceAction] = ToData.deriveEnum
    given ToData[ProposalProcedure] = ToData.deriveCaseClass[ProposalProcedure](0)
    given ToData[ScriptPurpose] = ToData.deriveEnum
    given ToData[ScriptInfo] = ToData.deriveEnum
    given ToData[TxInInfo] = ToData.deriveCaseClass[TxInInfo](0)
    given ToData[TxInfo] = ToData.deriveCaseClass[TxInfo](0)
    given ToData[ScriptContext] = ToData.deriveCaseClass[ScriptContext](0)
}

case class TxId(hash: ByteString)

case class TxOutRef(
    id: TxId,
    idx: BigInt
)

@Compile
object TxOutRef {
    given Eq[TxOutRef] = (a: TxOutRef, b: TxOutRef) => a.id.hash == b.id.hash && a.idx == b.idx
}

type Lovelace = BigInt
type ColdCommitteeCredential = Credential

type HotCommitteeCredential = Credential

type DRepCredential = Credential

enum DRep:
    case DRep(credential: DRepCredential)
    case AlwaysAbstain
    case AlwaysNoConfidence

enum Delegatee:
    case Stake(pubKeyHash: PubKeyHash)
    case Vote(dRep: DRep)
    case StakeVote(pubKeyHash: PubKeyHash, dRep: DRep)

enum TxCert:
    case RegStaking(credential: Credential, deposit: Option[Lovelace])
    case UnRegStaking(credential: Credential, refund: Option[Lovelace])
    case DelegStaking(credential: Credential, delegatee: Delegatee)
    case RegDeleg(credential: Credential, delegatee: Delegatee, deposit: Lovelace)
    case RegDRep(credential: DRepCredential, deposit: Lovelace)
    case UpdateDRep(credential: DRepCredential)
    case UnRegDRep(credential: DRepCredential, refund: Lovelace)
    case PoolRegister(poolId: PubKeyHash, poolVFR: PubKeyHash)
    case PoolRetire(pubKeyHash: PubKeyHash, epoch: BigInt)
    case AuthHotCommittee(cold: ColdCommitteeCredential, hot: HotCommitteeCredential)
    case ResignColdCommittee(cold: ColdCommitteeCredential)

enum Voter:
    case CommitteeVoter(credential: HotCommitteeCredential)
    case DRepVoter(credential: DRepCredential)
    case StakePoolVoter(pubKeyHash: PubKeyHash)

enum Vote:
    case No, Yes, Abstain

case class GovernanceActionId(txId: TxId, govActionIx: BigInt)

case class Committee(
    members: AssocMap[ColdCommitteeCredential, BigInt],
    quorum: BigInt
)

type Constitution = Option[ScriptHash]

case class ProtocolVersion(pvMajor: BigInt, pvMinor: BigInt)

type ChangedParameters = Data

enum GovernanceAction:
    case ParameterChange(
        id: Option[GovernanceActionId],
        parameters: ChangedParameters,
        constitutionScript: Option[ScriptHash]
    )
    case HardForkInitiation(id: Option[GovernanceActionId], protocolVersion: ProtocolVersion)
    case TreasuryWithdrawals(
        withdrawals: AssocMap[Credential, Lovelace],
        constitutionScript: Option[ScriptHash]
    )
    case NoConfidence(id: Option[GovernanceActionId])
    case UpdateCommittee(
        id: Option[GovernanceActionId],
        removedMembers: List[ColdCommitteeCredential],
        addedMembers: AssocMap[ColdCommitteeCredential, BigInt],
        newQuorum: Rational
    )
    case NewConstitution(id: Option[GovernanceActionId], constitution: Constitution)
    case InfoAction

case class ProposalProcedure(
    deposit: Lovelace,
    returnAddress: Credential,
    governanceAction: GovernanceAction
)

enum ScriptPurpose:
    case Minting(currencySymbol: CurrencySymbol)
    case Spending(txOutRef: TxOutRef)
    case Rewarding(credential: Credential)
    case Certifying(index: BigInt, cert: TxCert)
    case Voting(voter: Voter)
    case Proposing(index: BigInt, procedure: ProposalProcedure)

enum ScriptInfo:
    case MintingScript(currencySymbol: CurrencySymbol)
    case SpendingScript(txOutRef: TxOutRef, datum: Option[Datum] = Option.None)
    case RewardingScript(credential: Credential)
    case CertifyingScript(index: BigInt, cert: TxCert)
    case VotingScript(voter: Voter)
    case ProposingScript(index: BigInt, procedure: ProposalProcedure)

@deprecated("Use ScriptInfo instead")
case class SpendingScriptInfo(txOutRef: TxOutRef, datum: Option[Datum])

@deprecated("Use ScriptInfo instead")
case class MintingScriptInfo(currencySymbol: CurrencySymbol)

@deprecated("Use ScriptInfo instead")
case class RewardingScriptInfo(credential: Credential)

case class TxInInfo(
    outRef: TxOutRef,
    resolved: v2.TxOut
)

case class TxInfo(
    inputs: List[TxInInfo],
    referenceInputs: List[TxInInfo] = List.Nil,
    outputs: List[v2.TxOut] = List.Nil,
    fee: Lovelace = 0,
    mint: Value = Value.zero,
    certificates: List[TxCert] = List.Nil,
    withdrawals: AssocMap[Credential, Lovelace] = AssocMap.empty,
    validRange: Interval = Interval.always,
    signatories: List[PubKeyHash] = List.Nil,
    redeemers: AssocMap[ScriptPurpose, Redeemer] = AssocMap.empty,
    data: AssocMap[DatumHash, Datum] = AssocMap.empty,
    id: TxId,
    votes: AssocMap[Voter, AssocMap[GovernanceActionId, Vote]] = AssocMap.empty,
    proposalProcedures: List[ProposalProcedure] = List.Nil,
    currentTreasuryAmount: Option[Lovelace] = Option.None,
    treasuryDonation: Option[Lovelace] = Option.None
)

object TxInfo {
//    def placeholder: TxInfo = TxInfo()
}

case class ScriptContext(
    txInfo: TxInfo,
    redeemer: Redeemer = Data.unit,
    scriptInfo: ScriptInfo
)

@deprecated("Use ScriptContext instead")
case class SpendingScriptContext(
    txInfo: TxInfo,
    redeemer: Redeemer,
    scriptInfo: SpendingScriptInfo
)
