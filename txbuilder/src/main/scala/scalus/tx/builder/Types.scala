package scalus.tx.builder

import scalus.builtin.ByteString
import scalus.ledger.api.Timelock

import scala.collection.immutable

/** Representation of 28-byte hash value Used for addr_keyhash, script_hash, pool_keyhash, etc.
  */
case class Hash28(value: ByteString):
    require(value.size == 28, s"Hash28 must be 28 bytes long, got ${value.size}")

/** Representation of 32-byte hash value Used for transaction_id, block_body_hash, etc.
  */
case class Hash32(value: ByteString):
    require(value.size == 32, s"Hash32 must be 32 bytes long, got ${value.size}")

/** Verification key for transactions
  */
case class VKey(value: ByteString):
    require(value.size == 32, s"VKey must be 32 bytes long, got ${value.size}")

/** Digital signature (64 bytes)
  */
case class Signature(value: ByteString):
    require(value.size == 64, s"Signature must be 64 bytes long, got ${value.size}")

/** Key-evolving signature
  */
case class KesSignature(value: ByteString):
    require(value.size == 448, s"KesSignature must be 448 bytes long, got ${value.size}")

/** KES verification key
  */
case class KesVKey(value: ByteString):
    require(value.size == 32, s"KesVKey must be 32 bytes long, got ${value.size}")

/** VRF verification key
  */
case class VrfVKey(value: ByteString):
    require(value.size == 32, s"VrfVKey must be 32 bytes long, got ${value.size}")

/** VRF certificate consisting of two parts:
  *   - generic bytes (implementation specific)
  *   - 80 bytes output
  */
case class VrfCert(proof: ByteString, output: ByteString):
    require(output.size == 80, s"VrfCert output must be 80 bytes long, got ${output.size}")

/** Coin value representation
  */
case class Coin(value: Long):
    require(value >= 0, s"Coin value cannot be negative: $value")

/** Asset name (0-32 bytes)
  */
case class AssetName(value: ByteString):
    require(value.size <= 32, s"AssetName must be at most 32 bytes long, got ${value.size}")

/** Policy ID (same as script hash)
  */
type PolicyId = Hash28

/** Block number (8 bytes unsigned integer)
  */
case class BlockNo(value: Long):
    require(value >= 0, s"BlockNo cannot be negative: $value")

/** Slot number (8 bytes unsigned integer)
  */
case class SlotNo(value: Long):
    require(value >= 0, s"SlotNo cannot be negative: $value")

/** Transaction index (2 bytes unsigned integer)
  */
case class TransactionIndex(value: Int):
    require(
      value >= 0 && value <= 65535,
      s"TransactionIndex must be between 0 and 65535, got $value"
    )

/** Epoch number (8 bytes unsigned integer)
  */
case class EpochNo(value: Long):
    require(value >= 0, s"EpochNo cannot be negative: $value")

/** Epoch interval (4 bytes unsigned integer)
  */
case class EpochInterval(value: Int):
    require(value >= 0, s"EpochInterval cannot be negative: $value")

/** URL representation (0-128 chars)
  */
case class URL(value: String):
    require(value.length <= 128, s"URL must be at most 128 characters long, got ${value.length}")

/** DNS name (0-128 chars)
  */
case class DNSName(value: String):
    require(
      value.length <= 128,
      s"DNSName must be at most 128 characters long, got ${value.length}"
    )

/** Port number (0-65535)
  */
case class Port(value: Int):
    require(value >= 0 && value <= 65535, s"Port must be between 0 and 65535, got $value")

/** IPv4 address (4 bytes)
  */
case class IPv4(value: ByteString):
    require(value.size == 4, s"IPv4 must be 4 bytes long, got ${value.size}")

/** IPv6 address (16 bytes)
  */
case class IPv6(value: ByteString):
    require(value.size == 16, s"IPv6 must be 16 bytes long, got ${value.size}")

/** Unit interval (rational number between 0 and 1) Represented as [numerator, denominator]
  */
case class UnitInterval(numerator: Long, denominator: Long):
    require(denominator > 0, "Denominator must be positive")
    require(
      numerator <= denominator,
      s"Numerator ($numerator) must be <= denominator ($denominator)"
    )

/** Nonnegative interval Represented as [numerator, denominator] where denominator > 0
  */
case class NonnegativeInterval(numerator: Long, denominator: Long):
    require(denominator > 0, "Denominator must be positive")
    require(numerator >= 0, "Numerator must be non-negative")

/** Execution units
  */
case class ExUnits(mem: Long, steps: Long):
    require(mem >= 0, "Memory units must be non-negative")
    require(steps >= 0, "Step units must be non-negative")

/** Execution unit prices
  */
case class ExUnitPrices(memPrice: NonnegativeInterval, stepPrice: NonnegativeInterval)

/** Network ID
  */
enum NetworkId:
    case Testnet, Mainnet

/** Transaction metadata label (8 bytes unsigned integer)
  */
case class TransactionMetadatumLabel(value: Long):
    require(value >= 0, s"TransactionMetadatumLabel cannot be negative: $value")

/** Transaction metadata
  */
enum TransactionMetadatum:
    // Map from metadata to metadata
    case Map(items: scala.collection.immutable.Map[TransactionMetadatum, TransactionMetadatum])
    // List of metadata
    case List(items: Seq[TransactionMetadatum])
    // Integer value
    case Int(value: Long)
    // Bytes (0-64 bytes)
    case Bytes(value: ByteString)
    // Text (0-64 chars)
    case Text(value: String)

/** Metadata (map from label to metadatum)
  */
case class Metadata(items: Map[TransactionMetadatumLabel, TransactionMetadatum])

/** Anchor (URL + hash)
  */
case class Anchor(url: URL, dataHash: Hash32)

/** Bounded bytes (0-64 bytes)
  */
case class BoundedBytes(value: ByteString):
    require(value.size <= 64, s"BoundedBytes must be at most 64 bytes long, got ${value.size}")

/** Big integer representation for Plutus
  */
enum BigInt:
    case Int(value: scala.BigInt)
    case Uint(value: BoundedBytes)
    case Nint(value: BoundedBytes)

/** Plutus data
  */
type PlutusData = scalus.builtin.Data

/** Datum option for transaction output
  */
enum DatumOption:
    case Hash(value: Hash32)
    case Inline(value: PlutusData)

/** Credential type
  */
enum Credential:
    case KeyHash(value: Hash28)
    case ScriptHash(value: Hash28)

/** Script variants
  */
enum Script:
    case NativeScript(script: Timelock)
    case PlutusV1Script(script: ByteString)
    case PlutusV2Script(script: ByteString)
    case PlutusV3Script(script: ByteString)

/** Script reference
  */
case class ScriptRef(script: Script)

/** Native script variants
  */
enum NativeScript:
    case ScriptPubkey(keyHash: Hash28)
    case ScriptAll(scripts: Seq[NativeScript])
    case ScriptAny(scripts: Seq[NativeScript])
    case ScriptNOfK(n: Long, scripts: Seq[NativeScript])
    case InvalidBefore(slot: SlotNo)
    case InvalidHereafter(slot: SlotNo)

/** Multi-asset representation Map from policy ID to map from asset name to quantity
  */
case class MultiAsset[A](assets: Map[PolicyId, Map[AssetName, A]])

/** Mint representation (can have negative values)
  */
type Mint = MultiAsset[Long]

/** Value (ADA amount + optional multi-asset)
  */
enum Value:
    case Ada(coin: Coin)
    case MA(coin: Coin, assets: MultiAsset[Long])

/** Protocol version
  */
case class ProtocolVersion(major: Int, minor: Int):
    require(
      major >= 1 && major <= 10,
      s"Major protocol version must be between 1 and 10, got $major"
    )
    require(minor >= 0, s"Minor protocol version cannot be negative: $minor")

/** Operational certificate
  */
case class OperationalCert(
    hotVKey: KesVKey,
    sequenceNumber: Long,
    kesPeriod: Long,
    sigma: Signature
):
    require(sequenceNumber >= 0, s"Sequence number cannot be negative: $sequenceNumber")
    require(kesPeriod >= 0, s"KES period cannot be negative: $kesPeriod")

/** Host address in relay
  */
enum Relay:
    case SingleHostAddr(port: Option[Port], ipv4: Option[IPv4], ipv6: Option[IPv6])
    case SingleHostName(port: Option[Port], dnsName: DNSName)
    case MultiHostName(dnsName: DNSName)

/** Address (representation for different address types) Note: this is a simplified version as per
  * the provided schema
  */
case class Address(value: ByteString)

/** Reward account
  */
case class RewardAccount(value: ByteString)

/** Transaction input reference
  */
case class TransactionInput(id: Hash32, index: TransactionIndex)

/** Header body of a block
  */
case class HeaderBody(
    blockNumber: BlockNo,
    slot: SlotNo,
    prevHash: Option[Hash32],
    issuerVKey: VKey,
    vrfVKey: VrfVKey,
    vrfResult: VrfCert,
    blockBodySize: Int,
    blockBodyHash: Hash32,
    operationalCert: OperationalCert,
    protocolVersion: ProtocolVersion
):
    require(blockBodySize >= 0, s"Block body size cannot be negative: $blockBodySize")

/** Header of a block
  */
case class Header(body: HeaderBody, bodySignature: KesSignature)

/** Transaction output (Shelley style)
  */
case class ShelleyTransactionOutput(
    address: Address,
    amount: Value,
    datumHash: Option[Hash32]
)

/** Transaction output (Babbage style)
  */
case class BabbageTransactionOutput(
    address: Address,
    amount: Value,
    datum: Option[DatumOption],
    scriptRef: Option[ScriptRef]
)

/** Transaction output (either Shelley or Babbage style)
  */
enum TransactionOutput:
    case Shelley(output: ShelleyTransactionOutput)
    case Babbage(output: BabbageTransactionOutput)

/** Cost models for script languages
  */
case class CostModels(models: Map[Int, Seq[Long]])

/** Drep voting thresholds
  */
case class DrepVotingThresholds(
    motion: UnitInterval,
    committee: UnitInterval,
    hardFork: UnitInterval,
    ppNetworkGroup: UnitInterval,
    ppEconomicGroup: UnitInterval,
    ppTechnicalGroup: UnitInterval,
    ppGovernanceGroup: UnitInterval,
    treasuryWithdrawal: UnitInterval,
    stateOfNoConfidence: UnitInterval,
    updateToConstitution: UnitInterval
)

/** Pool voting thresholds
  */
case class PoolVotingThresholds(
    motion: UnitInterval,
    committee: UnitInterval,
    hardFork: UnitInterval,
    ppSecurityGroup: UnitInterval,
    treasuryWithdrawal: UnitInterval
)

/** Protocol parameter update
  */
case class ProtocolParamUpdate(
    minfeeA: Option[Coin] = None,
    minfeeB: Option[Coin] = None,
    maxBlockBodySize: Option[Int] = None,
    maxTransactionSize: Option[Int] = None,
    maxBlockHeaderSize: Option[Int] = None,
    keyDeposit: Option[Coin] = None,
    poolDeposit: Option[Coin] = None,
    maxEpoch: Option[EpochInterval] = None,
    nOpt: Option[Int] = None,
    poolPledgeInfluence: Option[NonnegativeInterval] = None,
    expansionRate: Option[UnitInterval] = None,
    treasuryGrowthRate: Option[UnitInterval] = None,
    minPoolCost: Option[Coin] = None,
    adaPerUtxoByte: Option[Coin] = None,
    costModels: Option[CostModels] = None,
    executionCosts: Option[ExUnitPrices] = None,
    maxTxExUnits: Option[ExUnits] = None,
    maxBlockExUnits: Option[ExUnits] = None,
    maxValueSize: Option[Int] = None,
    collateralPercentage: Option[Int] = None,
    maxCollateralInputs: Option[Int] = None,
    poolVotingThresholds: Option[PoolVotingThresholds] = None,
    drepVotingThresholds: Option[DrepVotingThresholds] = None,
    minCommitteeSize: Option[Int] = None,
    committeeTermLimit: Option[EpochInterval] = None,
    governanceActionValidityPeriod: Option[EpochInterval] = None,
    governanceActionDeposit: Option[Coin] = None,
    drepDeposit: Option[Coin] = None,
    drepInactivityPeriod: Option[EpochInterval] = None,
    minFeeRefScriptCoinPerByte: Option[NonnegativeInterval] = None
):
    // Validation of int parameters
    maxBlockBodySize.foreach(v => require(v > 0, s"maxBlockBodySize must be positive, got $v"))
    maxTransactionSize.foreach(v => require(v > 0, s"maxTransactionSize must be positive, got $v"))
    maxBlockHeaderSize.foreach(v => require(v > 0, s"maxBlockHeaderSize must be positive, got $v"))
    nOpt.foreach(v => require(v > 0, s"nOpt must be positive, got $v"))
    maxValueSize.foreach(v => require(v > 0, s"maxValueSize must be positive, got $v"))
    collateralPercentage.foreach(v =>
        require(v > 0, s"collateralPercentage must be positive, got $v")
    )
    maxCollateralInputs.foreach(v =>
        require(v > 0, s"maxCollateralInputs must be positive, got $v")
    )
    minCommitteeSize.foreach(v => require(v > 0, s"minCommitteeSize must be positive, got $v"))

/** VKEY witness
  */
case class VKeyWitness(vkey: VKey, signature: Signature)

/** Bootstrap witness
  */
case class BootstrapWitness(
    publicKey: VKey,
    signature: Signature,
    chainCode: ByteString,
    attributes: ByteString
):
    require(chainCode.size == 32, s"Chain code must be 32 bytes long, got ${chainCode.size}")

/** Redeemer tag
  */
enum RedeemerTag:
    case Spend, Mint, Cert, Reward, Voting, Proposing

/** Redeemer for Plutus execution
  */
case class Redeemer(
    tag: RedeemerTag,
    index: Int,
    data: PlutusData,
    exUnits: ExUnits
):
    require(index >= 0, s"Redeemer index cannot be negative: $index")

/** Redeemers - either flat array or map format
  */
enum Redeemers:
    case FlatArray(redeemers: Seq[Redeemer])
    case Map(redeemers: immutable.Map[(RedeemerTag, Int), (PlutusData, ExUnits)])

/** Transaction witness set
  */
case class TransactionWitnessSet(
    vkeyWitnesses: Option[Set[VKeyWitness]] = None,
    nativeScripts: Option[Set[NativeScript]] = None,
    bootstrapWitnesses: Option[Set[BootstrapWitness]] = None,
    plutusV1Scripts: Option[Set[ByteString]] = None,
    plutusData: Option[Set[PlutusData]] = None,
    redeemers: Option[Redeemers] = None,
    plutusV2Scripts: Option[Set[ByteString]] = None,
    plutusV3Scripts: Option[Set[ByteString]] = None
):
    // Ensure non-empty sets
    vkeyWitnesses.foreach(s => require(s.nonEmpty, "VKey witnesses set cannot be empty"))
    nativeScripts.foreach(s => require(s.nonEmpty, "Native scripts set cannot be empty"))
    bootstrapWitnesses.foreach(s => require(s.nonEmpty, "Bootstrap witnesses set cannot be empty"))
    plutusV1Scripts.foreach(s => require(s.nonEmpty, "Plutus V1 scripts set cannot be empty"))
    plutusData.foreach(s => require(s.nonEmpty, "Plutus data set cannot be empty"))
    plutusV2Scripts.foreach(s => require(s.nonEmpty, "Plutus V2 scripts set cannot be empty"))
    plutusV3Scripts.foreach(s => require(s.nonEmpty, "Plutus V3 scripts set cannot be empty"))

/** Auxiliary data (metadata)
  */
enum AuxiliaryData:
    case JustMetadata(metadata: Metadata)
    case ShelleyMA(metadata: Metadata, auxiliaryScripts: Seq[NativeScript])
    case Alonzo(
        metadata: Option[Metadata] = None,
        nativeScripts: Option[Seq[NativeScript]] = None,
        plutusV1Scripts: Option[Seq[ByteString]] = None,
        plutusV2Scripts: Option[Seq[ByteString]] = None,
        plutusV3Scripts: Option[Seq[ByteString]] = None
    )

/** Transaction body
  */
case class TransactionBody(
    inputs: Set[TransactionInput],
    outputs: Seq[TransactionOutput],
    fee: Coin,
    ttl: Option[SlotNo] = None,
    auxiliaryDataHash: Option[Hash32] = None,
    validityStartInterval: Option[SlotNo] = None,
    mint: Option[Mint] = None,
    scriptDataHash: Option[Hash32] = None,
    collateral: Option[Set[TransactionInput]] = None,
    requiredSigners: Option[Set[Hash28]] = None,
    networkId: Option[NetworkId] = None,
    collateralReturn: Option[TransactionOutput] = None,
    totalCollateral: Option[Coin] = None,
    referenceInputs: Option[Set[TransactionInput]] = None,
    currentTreasuryValue: Option[Coin] = None,
    donation: Option[Long] = None
):
    // Ensure non-empty sets for collateral and reference inputs
    collateral.foreach(c => require(c.nonEmpty, "Collateral inputs set cannot be empty"))
    requiredSigners.foreach(s => require(s.nonEmpty, "Required signers set cannot be empty"))
    referenceInputs.foreach(r => require(r.nonEmpty, "Reference inputs set cannot be empty"))

    // Validate positive values
    donation.foreach(v => require(v > 0, s"Positive scalars must be greater than 0, got $v"))

/** Transaction
  */
case class Transaction(
    body: TransactionBody,
    witnessSet: TransactionWitnessSet,
    isValid: Boolean,
    auxiliaryData: Option[AuxiliaryData] = None
)

/** Block
  */
case class Block(
    header: Header,
    transactionBodies: Seq[TransactionBody],
    transactionWitnessSets: Seq[TransactionWitnessSet],
    auxiliaryDataSet: Map[TransactionIndex, AuxiliaryData],
    invalidTransactions: Seq[TransactionIndex]
):
    // The length of transaction_bodies and transaction_witness_sets must be the same
    require(
      transactionBodies.size == transactionWitnessSets.size,
      s"Number of transaction bodies (${transactionBodies.size}) must equal number of witness sets (${transactionWitnessSets.size})"
    )

    // Every transaction_index must be strictly smaller than the length of transaction_bodies
    invalidTransactions.foreach { idx =>
        require(
          idx.value < transactionBodies.size,
          s"Invalid transaction index ${idx.value} must be less than the number of transaction bodies (${transactionBodies.size})"
        )
    }
    auxiliaryDataSet.keys.foreach { idx =>
        require(
          idx.value < transactionBodies.size,
          s"Auxiliary data index ${idx.value} must be less than the number of transaction bodies (${transactionBodies.size})"
        )
    }
