package scalus.patterns

import scalus.Compiler.compile
import scalus.builtin.Builtins
import scalus.builtin.ByteString
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.FromData
import scalus.builtin.Data.ToData
import scalus.cardano.blueprint.Application
import scalus.cardano.blueprint.Blueprint
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.{show as _, *}

case class Config(
    init: TxOutRef,
    deadline: PosixTime,
    penalty: Address
) derives FromData,
      ToData

@Compile
object Config

type NodeKey = Option[TokenName]

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
object SetNode:
    given Eq[SetNode] = (x, y) => x.key === y.key && x.link === y.link

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
object NodePair:
    given Eq[NodePair] = (x, y) => x.value === y.value && x.node === y.node

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

@Compile
object LinkedList extends DataParameterizedValidator:

    val nodeToken: TokenName = fromString("LAN") // FIXME: utf8"LAN" syntax

    def mkCommon(
        currencySymbol: CurrencySymbol,
        tx: TxInfo
    ): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        val fromNodeOutputs =
            tx.inputs.map(_.resolved).filter(_.value.toSortedMap.contains(currencySymbol))
        val toNodeOutputs = tx.outputs.filter(_.value.toSortedMap.contains(currencySymbol))
        val allNodeOutputs = fromNodeOutputs ++ toNodeOutputs
        // TODO: reduce computational complexity
        allNodeOutputs.headOption match
            case Some(head) =>
                require(
                  allNodeOutputs.forall(head.address === _.address),
                  "All outputs must have same address"
                )
                val nodeInputs = fromNodeOutputs.map(out =>
                    NodePair(
                      out.value,
                      out.datum match
                          case OutputDatum.OutputDatum(nodeDatum) =>
                              val node = nodeDatum.to[SetNode]
                              node
                          case _ => fail("Node datum must be inline")
                    )
                )
                val nodeOutputs = toNodeOutputs.map(out =>
                    NodePair(
                      out.value,
                      out.datum match
                          case OutputDatum.OutputDatum(nodeDatum) =>
                              val node = nodeDatum.to[SetNode]
                              out.value.tokens(currencySymbol).toList match
                                  case List.Cons(tokenAmount, tail) =>
                                      tail match
                                          case List.Nil =>
                                              val (token, amount) = tokenAmount
                                              val nodeKey = parseNodeKey(token)
                                              require(
                                                nodeKey === node.key &&
                                                    out.value.flatten.length == BigInt(2) &&
                                                    node.key.forall(key =>
                                                        node.link.forall(key < _)
                                                    ),
                                                "validate node output utxo"
                                              )
                                              // TODO: secod pass over output
                                              // values, can be done in single
                                              require(
                                                out.value.flatten.filterMap {
                                                    case (currency, tokenName, _) =>
                                                        if tokenName.take(nodeToken.length) ===
                                                                nodeToken
                                                        then Some(currency)
                                                        else None
                                                } === List.single(currencySymbol),
                                                "There's must be exactly one output by token prefix, with expected currency"
                                              )
                                          case _ =>
                                              fail(
                                                "Token key output for the currency must be single"
                                              )
                                  case _ =>
                                      fail("There's must be a token key for the currency output")
                              node
                          case _ => fail("Node datum must be inline")
                    )
                )
                val common = Common(currencySymbol, tx.mint, nodeInputs, nodeOutputs)
                (common, tx.inputs, tx.outputs, tx.signatories, tx.validRange)
            case _ => fail("There's must be at least one output")

    def parseNodeKey(token: TokenName): NodeKey =
        require(
          token.take(nodeToken.length) === nodeToken,
          "validate node token prefix"
        )
        if nodeToken.length < token.length
        then Some(token.drop(nodeToken.length))
        else None

    override def mint(
        cfgData: Data,
        redeemer: Data,
        currencySymbol: CurrencySymbol,
        tx: TxInfo
    ): Unit =
        val cfg = cfgData.to[Config]
        val (common, inputs, outputs, signatories, range) = mkCommon(currencySymbol, tx)
        redeemer.to[NodeAction] match
            case NodeAction.Init =>
                require(
                  inputs.exists(cfg.init === _.outRef) && init(common),
                  "validate init"
                )
            case NodeAction.Deinit =>
                require(deinit(common), "validate deinit")
            case NodeAction.Insert(key, covering) =>
                require(range.isEntirelyBefore(cfg.deadline), "must be before the deadline")
                require(signatories.contains(key), "must be signed by a node key")
                require(insert(common, key, covering), "validate insert")
            case NodeAction.Remove(key, covering) =>
                require(range.isEntirelyBefore(cfg.deadline), "must be before the deadline")
                require(
                  remove(common, range, cfg, outputs, signatories, key, covering),
                  "validate remove"
                )

    // State transition handlers (used in list minting policy)
    // aka list natural transformations

    /** Initialize an empty unordered list.
      *
      * Application code must ensure that this action can happen only once.
      */
    def init(common: Common): Boolean =
        val mustSpendNodes = common.inputs.isEmpty // TODO: early exit
        val mustExactlyOneNodeOutput = common.outputs.length == BigInt(1)
        val mustMintCorrectly =
            common.mint.flatten === List.single((common.currency, nodeToken, BigInt(1)))
        mustSpendNodes && mustExactlyOneNodeOutput && mustMintCorrectly

    /** Deinitialize an empty unordered list.
      */
    def deinit(common: Common): Boolean =
        common.inputs match
            case List.Cons(headNode, tail) =>
                tail match
                    case List.Nil =>
                        headNode.node.link match
                            case None =>
                                val mustNotProduceNodeOutput = common.outputs.isEmpty
                                val mustBurnCorrectly = common.mint.flatten === List.single(
                                  (common.currency, nodeToken, BigInt(-1))
                                )
                                mustNotProduceNodeOutput && mustBurnCorrectly
                            case _ => false // Linked list must be empty
                    case _ => false // Head node input must be single
            case _ => false // There must be a head node input

    /** Insert a node under for a key at an unordered list.
      *
      * WARNING: An application using a key-unordered list MUST only add nodes with unique keys to
      * the list. Duplicate keys break the linked list data structure.
      */
    def insert(common: Common, insertKey: PubKeyHash, node: SetNode): Boolean =
        val PubKeyHash(key) = insertKey
        val mustCoverInsertingKey = node.key.forall(_ < key) && node.link.forall(_ > key)
        common.inputs match
            case List.Cons(coveringNode, tail) =>
                tail match
                    case List.Nil =>
                        val mustHasDatumInOutput =
                            common.outputs.exists(SetNode(Some(key), node.link) === _.node)
                        val mustCorrectNodeOutput = common.outputs.contains(
                          NodePair(coveringNode.value, SetNode(node.key, Some(key)))
                        )
                        val mustMintCorrect = common.mint.flatten === List.single(
                          (common.currency, nodeToken ++ key, BigInt(1)) // serializedKey
                        )
                        mustCoverInsertingKey && mustHasDatumInOutput && mustCorrectNodeOutput && mustMintCorrect
                    case _ => false // Linked list must be empty
            case _ => false // Covering node input must be single

    /** Remove a non-root node from the list.
      */
    def remove(
        common: Common,
        range: Interval,
        config: Config,
        outputs: List[TxOut],
        signatories: List[PubKeyHash],
        removeKey: PubKeyHash,
        node: SetNode
    ): Boolean =
        val PubKeyHash(key) = removeKey
        val mustCoverRemoveKey = node.key.forall(_ < key) && node.link.forall(_ > key)
        val mustSpendTwoNodes = common.inputs.length == BigInt(2)
        common.inputs.find(SetNode(node.key, Some(key)) === _.node) match
            case Some(stayNode) =>
                val mustCorrectNodeOutput = common.outputs.contains(
                  NodePair(stayNode.value, node)
                )
                val mustMintCorrect = common.mint.flatten === List.single(
                  (common.currency, nodeToken ++ key, BigInt(-1)) // serializedKey
                )
                val mustSignByUser = signatories.contains(removeKey)
                // TODO: both find in a single pass
                common.inputs.find(SetNode(Some(key), node.link) === _.node) match
                    case Some(removeNode) =>
                        val div = removeNode.value.getLovelace / BigInt(4)
                        val rem = removeNode.value.getLovelace % BigInt(4)
                        val fee = if rem == BigInt(0) then div else div + BigInt(1)
                        val mustSatisfyRemovalBrokePhaseRules =
                            range.isEntirelyBefore(config.deadline) || outputs.exists(out =>
                                out.address === config.penalty && fee < out.value.getLovelace
                            )
                        mustCoverRemoveKey && mustSpendTwoNodes && mustCorrectNodeOutput && mustMintCorrect && mustSignByUser && mustSatisfyRemovalBrokePhaseRules
                    case _ => false // There must be a remove node input
            case _ => false // There must be a stay node input

object LinkedListContract:

    inline def compiled(using scalus.Compiler.Options) = compile(LinkedList.validate)

    def application: Application = Application
        .ofSingleValidator[Config, NodeAction](
          "LinkedList validator",
          "Linked list structures leverage the EUTXO model to enhancing scalability and throughput significantly. By linking multiple UTXOs together through a series of minting policies and validators, it can improve the user experience interacting with smart contract concurrently.",
          "1.0.0",
          LinkedList.validate
        )

    def blueprint: Blueprint = application.blueprint
