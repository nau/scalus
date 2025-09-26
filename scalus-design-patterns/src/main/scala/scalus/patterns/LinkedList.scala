package scalus.examples

import scalus.Compiler.compile
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.cardano.plutus.contract.blueprint.Application
import scalus.cardano.plutus.contract.blueprint.Blueprint
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.{show as _, *}

case class AssetClass(
    currencySymbol: CurrencySymbol,
    tokenName: TokenName
) derives FromData,
      ToData

@Compile
object AssetClass

case class Config(
    init: TxOutRef,
    deadline: PosixTime,
    penalty: Address
) derives FromData,
      ToData

@Compile
object Config

type NodeKey = Option[PubKeyHash]

/** Node key and reference:
  *
  * @param key
  *   Has unique key (unless it's the root node)
  * @param link
  *   Has a unique link to another node's key (unless it's the last node of the list)
  */
case class SetNode(
    key: NodeKey,
    link: NodeKey,
) derives FromData,
      ToData

@Compile
object SetNode

/** Every list node utxo:
  *
  * @param value
  *   Holds some app-specific value
  * @param node
  *   Node state
  */
case class NodePair(
    value: Value, // ???: data: Data
    node: SetNode
) derives FromData,
      ToData

@Compile
object NodePair

/** Common information shared between all redeemers:
  *
  * @param currency
  *   state token (own) currency symbol
  * @param mint
  *   value minted in current Tx
  * @param inputs
  *   current Tx inputs
  * @param outputs
  *   current Tx outputs
  */
case class Common(
    currency: CurrencySymbol,
    mint: Value,
    inputs: List[NodePair], // TxInInfo
    outputs: List[NodePair] // TxOut
) derives FromData,
      ToData

@Compile
object Common

enum NodeAction derives FromData, ToData:
    case Init
    case Deinit
    case Insert(key: PubKeyHash, covering: SetNode)
    case Remove(key: PubKeyHash, covering: SetNode)

@Compile
object NodeAction

case class LinkedList(cfg: Config) extends Validator:

    def mkCommon(tx: TxInfo): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        ???

    override def mint(
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        tx: TxInfo
    ): Unit =
        val (common, inputs, _outputs, _sigs, range) = mkCommon(tx)
        redeemer.to[NodeAction] match
            case NodeAction.Init =>
                require(
                  inputs.any(cfg.init === _.outRef) && init(common),
                  "validate init"
                )
            case NodeAction.Deinit                => fail("unimplemented")
            case NodeAction.Insert(key, covering) => fail("unimplemented")
            case NodeAction.Remove(key, covering) => fail("unimplemented")

    // State transition handlers (used in list minting policy)
    // aka list natural transformations

    /*
    private def serializedKey(key: NodeKey = None): ByteString =
        key.map(nodePrefix ++ _).getOrElse(nodePrefix)

    /** Initialize an empty unordered list.
     *
      * Application code must ensure that this action can happen only once.
     */
    def init(
        outputs: List[TxOut], // current Tx outputs
        minted: SortedMap[TokenName, BigInt], // value minted in current Tx
        currency: CurrencySymbol // state token (own) currency
    ): Boolean =
        // 1. The transaction's sole effect on the list is to add the root key.
        require( // FIXME: can be done in a single pass
          minted.size === BigInt(1) &&
              minted.get(serializedKey()) === Some(BigInt(1)),
          "Key added"
        )
        // 2. The list must be empty after the transaction, as proved by an output
        // root_node that holds the minted root node NFT.
        val rootNode = outputs.head
        rootNode.datum match
            case OutputDatum.OutputDatum(outputDatum) =>
                Builtins.unConstrData(outputDatum)
            case _ => fail("Node datum must be inline")

        // 3. The root_node must not contain any other non-ADA tokens.
        fail("unimplemented")
     */

    val nodeToken: CurrencySymbol = b"LAN"
    val nodePrefix: PubKeyHash = PubKeyHash(nodeToken)

    /** Initialize an empty unordered list.
      *
      * Application code must ensure that this action can happen only once.
      */
    def init(common: Common): Boolean =
        val mustSpendNodes = common.inputs.isEmpty
        val mustExactlyOneNodeOutput = common.outputs.length === BigInt(1)
        val mustMintCorrectly =
            common.mint.flatten === List.single((common.currency, nodeToken, BigInt(1)))
        mustSpendNodes && mustExactlyOneNodeOutput && mustMintCorrectly

    /** Deinitialize an empty unordered list.
      */
    def deinit: Boolean = fail("unimplemented")

    /** Prepend a new node to the beginning of the list.
      *
      * WARNING: An application using a key-unordered list MUST only add nodes with unique keys to
      * the list. Duplicate keys break the linked list data structure.
      *
      * The index arguments in this function are relative to the node_inputs and node_outputs. They
      * are NOT absolute.
      */
    def prependUnsafe: Boolean = fail("unimplemented")

    /** Append a new node to the end of the list.
      *
      * WARNING: An application using a key-unordered list MUST only add nodes with unique keys to
      * the list. Duplicate keys break the linked list data structure.
      *
      * The index arguments in this function are relative to the node_inputs
      */
    def appendUnsafe: Boolean = fail("unimplemented")

    /** Remove a non-root node from the list.
      *
      * The index arguments in this function are relative to the node_inputs and node_outputs. They
      * are NOT absolute.
      */
    def remove: Boolean = fail("unimplemented")

    // ???: insert

@Compile
object LinkedList
