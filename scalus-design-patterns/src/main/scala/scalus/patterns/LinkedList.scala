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

/** Linked list configuration parameter
  *
  * @param init
  *   An input reference that makes each initialized linked list unique.
  * @param deadline
  *   The deadline to which royalties could be payed.
  * @param penalty
  *   A payment address to pay royalties.
  */
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
  * Passed at redeemer, supposed to be stored in tx's datum.
  *
  * @param key
  *   Has unique key (unless it's the root node).
  *   - [[scalus.prelude.Option.None]] means a head of the linked list
  *   - [[scalus.prelude.Option.Some]] means a node of the linked list.
  * @param ref
  *   Has a unique link to another node's key (unless it's the last node of the list)
  *   - [[scalus.prelude.Option.None]] means a end of the linked list (or empty head)
  *   - [[scalus.prelude.Option.Some]] contains a reference key to the next node.
  */
case class Cons(
    key: NodeKey, // FIXE: storing key in datum is redundancy, available as a token at value already
    // TODO: store extra `data: Data`
    ref: NodeKey = None
) derives FromData,
      ToData

@Compile
object Cons:
    inline def head(ref: NodeKey = None): Cons = Cons(key = None, ref)
    inline def cons(key: TokenName, ref: NodeKey = None): Cons = Cons(key = Some(key), ref)

    given Eq[Cons] = (x, y) => x.key === y.key && x.ref === y.ref

/** Every linked list node UTxO:
  *
  * Representation of an [[scalus.ledger.api.v2.TxOut]] of the node for the
  * [[scalus.patterns.Common]] argument with tx's inputs and outputs.
  *
  * @param value
  *   Holds node's token key.
  * @param cell
  *   Node state, [[scalus.patterns.Cons]] keys.
  */
case class Node(
    value: Value,
    cell: Cons
) derives FromData,
      ToData

@Compile
object Node:
    given Eq[Node] = (x, y) => x.value === y.value && x.cell === y.cell

/** Common information shared between all redeemers:
  *
  * @param policy
  *   state token (own) policy id.
  * @param mint
  *   value minted in current Tx, meant to contain a policy's node token.
  * @param inputs
  *   current Tx inputs, the [[scalus.patterns.NodePair]] of [[scalus.ledger.api.v2.TxOut]].
  * @param outputs
  *   current Tx outputs, the [[scalus.patterns.NodePair]] of [[scalus.ledger.api.v3.TxInInfo]].
  */
case class Common(
    policy: PolicyId,
    mint: Value,
    inputs: List[Node],
    outputs: List[Node]
) derives FromData,
      ToData

@Compile
object Common

/** Actions that can be performed on the linked list */
enum NodeAction derives FromData, ToData:
    /** Initialize linked list head reference cell with current [[scalus.patterns.Config]] */
    case Init

    /** Burn an empty linked list head reference, pay royalties */
    case Deinit

    /** Insert `key` at `covering`
      *
      * @param key
      * @param covering
      */
    case Insert(key: PubKeyHash, covering: Cons)

    /** Remove `key` at `covering`
      *
      * @param key
      * @param covering
      */
    case Remove(key: PubKeyHash, covering: Cons)

@Compile
object NodeAction

/** Linked List is an on-chain, sorted linked list solution designed for blockchain environments,
  * specifically utilizing NFTs (Non-Fungible Tokens) and datums. It provides a structured and
  * efficient way to store and manipulate a list of key/value pairs on-chain.
  *
  * @see
  *   [[https://github.com/Anastasia-Labs/aiken-linked-list/tree/0.0.1?tab=readme-ov-file#linked-list Aiken Linked List]]
  * @see
  *   [[https://github.com/Anastasia-Labs/data-structures/blob/2bbe6e7388d3fb0fa5c0e5cbfcaad98294869655/pages/linked_list.mdx#introduction-to-linked-list Plutarch Linked List Guide]]
  *
  * @example
  *   {{{
  *   ╭──────╮  ╭───────╮  ╭────────╮  ╭────────╮  ╭───────╮
  *   │•Head•├─>│ Apple ├─>│ Banana ├─>│ Orange ├─>│ Peach │
  *   ╰──────╯  ╰───────╯  ╰────────╯  ╰────────╯  ╰───────╯
  *   }}}
  *
  * ==Entry Structure==
  *
  * Each entry in the list comprises:
  *
  *   - [[scalus.ledger.api.v1.TokenName]] '''NFT''': A unique identifier for each entry.
  *   - [[scalus.patterns.Cons]] '''Datum''': A data structure containing the key/value pair, a
  *     reference to the entry's NFT, and a pointer to the next NFT in the list.
  *
  * ==Operations==
  *
  *   - '''Inserting an Entry'''
  *
  * Insertion involves:
  *
  *   - '''Inputs''': Two adjacent list entries.
  *   - '''Outputs''':
  *     - The first input entry, modified to point to the new entry.
  *     - The newly inserted entry, pointing to the second input entry.
  *     - The second input entry, unchanged.
  *
  * Validation Rules
  *
  *   - Keys must maintain the order: `a < b < c`, where `a` is the lowest, `b` is the new key, and
  *     `c` is the highest.
  *   - The pointers must be correctly updated to maintain list integrity.
  *
  * @example
  *   {{{
  *     ╭──────╮  ╭───────╮  ╭────────╮  ╭────────╮  ╭───────╮
  *     │•Head•├─>│ Apple ├─>│ Banana ├─>│ Orange ├─>│ Peach │
  *     ╰──────╯  ╰───────╯  ╰─────┬──╯  ╰────┬───╯  ╰───────╯
  *                                │          │
  *                            ┏━━━V━━━━━━━━━━V━━━━━━━━━━━━━━┓
  *                            ┃▓█▓▒░ Insert Transaction ░▒▓█┃
  *                            ┗━━━┯━━━━━━━━━━┯━━━━━━━━━━┯━━━┛
  *                                │          │          │
  *       ╭──────╮  ╭───────╮  ╭───V────╮  ╭──V───╮  ╭───V────╮  ╭───────╮
  *       │•Head•├─>│ Apple ├─>│:Banana:├─>│~Kiwi~├─>│ Orange ├─>│ Peach │
  *       ╰──────╯  ╰───────╯  ╰────────╯  ╰──────╯  ╰────────╯  ╰───────╯
  *   }}}
  *
  *   - '''Removing an Entry'''
  *
  * To remove an entry:
  *
  *   - '''Inputs''': The entry to remove and its preceding entry.
  *   - '''Output''': The preceding entry is modified to point to what the removed entry was
  *     pointing to.
  *
  * @example
  *   {{{
  *       ╭──────╮  ╭───────╮  ╭────────╮  ╭──────╮  ╭────────╮  ╭───────╮
  *       │•Head•├─>│ Apple ├─>│ Banana ├─>│~Kiwi~├─>│ Orange ├─>│ Peach │
  *       ╰──────╯  ╰───────╯  ╰───┬────╯  ╰──┬───╯  ╰────────╯  ╰───────╯
  *                                │          │
  *                            ┏━━━V━━━━━━━━━━V━━━━━━━━━━━━━━┓
  *                            ┃▓█▓▒░ Delete Transaction ░▒▓█┃
  *                            ┗━━━┯━━━━━━━━━━━━━━━━━━━━━━━━━┛
  *                                │
  *     ╭──────╮  ╭───────╮  ╭─────V──╮  ╭────────╮  ╭───────╮
  *     │•Head•├─>│ Apple ├─>│:Banana:├─>│ Orange ├─>│ Peach │
  *     ╰──────╯  ╰───────╯  ╰────────╯  ╰────────╯  ╰───────╯
  *   }}}
  */
@Compile
object LinkedList extends DataParameterizedValidator:

    def nodeToken(token: TokenName = fromString("")): TokenName =
        fromString("LAN") ++ token // FIXME: utf8"LAN" syntax

    def isNodeToken(token: TokenName): Boolean = token.take(nodeToken().length) === nodeToken()

    def nodeKey(token: TokenName): NodeKey =
        Some(token.drop(nodeToken().length)).filter(_.nonEmpty)

    def mkCommon(
        policy: PolicyId,
        tx: TxInfo
    ): (Common, List[TxInInfo], List[TxOut], List[PubKeyHash], Interval) =
        val fromNodeOutputs =
            tx.inputs.map(_.resolved).filter(_.value.toSortedMap.contains(policy))
        val toNodeOutputs = tx.outputs.filter(_.value.toSortedMap.contains(policy))
        val allNodeOutputs =
            fromNodeOutputs ++ toNodeOutputs // TODO: reduce computational complexity

        allNodeOutputs.headOption match
            case Some(head) =>
                require(
                  allNodeOutputs.forall(head.address === _.address),
                  "All outputs must have same address"
                )
                val nodeInputs = fromNodeOutputs.map(getNodePair)
                val nodeOutputs = toNodeOutputs.map(out =>
                    val nodePair = getNodePair(out)
                    validateNode(policy, nodePair)
                    nodePair
                )
                val common = Common(policy, tx.mint, nodeInputs, nodeOutputs)
                (common, tx.inputs, tx.outputs, tx.signatories, tx.validRange)
            case _ => fail("There's must be at least one output")

    def getNodePair(out: TxOut): Node = Node(
      out.value,
      out.datum match
          case OutputDatum.OutputDatum(nodeDatum) => nodeDatum.to[Cons]
          case _                                  => fail("Node datum must be inline")
    )

    def validateNode(policy: PolicyId, node: Node): Unit =
        val Node(value, cell) = node
        value.flatten match
            case List.Cons(adaPolicyTokenAmount, withoutLovelace) =>
                val (adaPolicy, adaToken, _amount) = adaPolicyTokenAmount
                // require(
                //   adaPolicy === Value.adaPolicyId && adaToken === Value.adaTokenName,
                //   "There's must be a lovelace value for each policy output"
                // )
                // ???: a bit weird requirement that non-policy value can not has token name
                //
                require(
                  (adaPolicy !== policy) && !isNodeToken(adaToken),
                  "There's must be a non-policy value for each policy output"
                )

                withoutLovelace match
                    case List.Cons(policyTokenAmount, tail) =>
                        tail match
                            case List.Nil =>
                                val (policyId, token, amount) = policyTokenAmount
                                require( // INFO: this check is redundancy
                                  policyId === policy,
                                  "There's must be a token key for the policy output"
                                )
                                require(
                                  amount == BigInt(1),
                                  "Minted token be exactly one per output"
                                )
                                require(
                                  isNodeToken(token),
                                  "validate node token"
                                )
                                require(
                                  nodeKey(token) === cell.key,
                                  "Datum must contain a valid node key"
                                )
                                // ???: order is a weird requirement for public keys
                                require(
                                  cell.key.forall(key => cell.ref.forall(key < _)),
                                  "Nodes must be ordered by keys"
                                )
                            case _ => fail("Tere's must be no extra values the policy output")
                    case _ => fail("There's must be a token key for the policy output")
            case _ => fail("There's must be a lovelace value for each policy output")

    override def mint(
        cfgData: Data,
        redeemer: Data,
        policy: PolicyId,
        tx: TxInfo
    ): Unit =
        val cfg = cfgData.to[Config]
        val (common, inputs, outputs, signatories, range) = mkCommon(policy, tx)

        redeemer.to[NodeAction] match
            case NodeAction.Init =>
                require(
                  inputs.exists(cfg.init === _.outRef),
                  "The head must be unique: the initial UTxO must be spent"
                )
                init(common)
            case NodeAction.Deinit =>
                deinit(common)
            case NodeAction.Insert(key, covering) =>
                require(
                  range.isEntirelyBefore(cfg.deadline),
                  "must be before the deadline"
                )
                require(signatories.contains(key), "must be signed by a node key")
                insert(common, key, covering)
            case NodeAction.Remove(key, covering) =>
                require(
                  range.isEntirelyBefore(cfg.deadline),
                  "must be before the deadline"
                )
                remove(common, range, cfg, outputs, signatories, key, covering)

    // State transition handlers (used in list minting policy)
    // aka list natural transformations

    /** Initialize an empty unordered list.
      *
      * Application code must ensure that this action can happen only once.
      */
    def init(common: Common): Unit =
        require(
          common.inputs.isEmpty,
          "Must spend nodes"
        )
        require(
          common.outputs.length == BigInt(1),
          "Must exactly one node output"
        )
        require(
          common.mint.flatten === List.single((common.policy, nodeToken(), BigInt(1))),
          "Must mint correctly"
        )

    /** Deinitialize an empty unordered list.
      */
    def deinit(common: Common): Unit =
        common.inputs match
            case List.Cons(headNode, tail) =>
                tail match
                    case List.Nil =>
                        headNode.cell.ref match
                            case None =>
                                require(
                                  common.outputs.isEmpty,
                                  "Must not produce node output"
                                )
                                require(
                                  common.mint.flatten === List.single(
                                    (common.policy, nodeToken(), BigInt(-1))
                                  ),
                                  "Must burn correctly"
                                )
                            case _ => fail("Linked list must be empty")
                    case _ => fail("Head node input must be single")
            case _ => fail("There must be a head node input")

    import Cons.cons

    /** Insert a node under for a key at an unordered list.
      *
      * WARNING: An application using a key-unordered list MUST only add nodes with unique keys to
      * the list. Duplicate keys break the linked list data structure.
      */
    def insert(common: Common, insertKey: PubKeyHash, cell: Cons): Unit =
        val PubKeyHash(key) = insertKey
        require(
          cell.key.forall(_ < key) && cell.ref.forall(_ > key), // ???: logic???
          "Must cover inserting key"
        )
        common.inputs match
            case List.Cons(coveringNode, tail) =>
                tail match
                    case List.Nil =>
                        require(
                          common.outputs.exists(cons(key, cell.ref) === _.cell),
                          "Must has datum in output"
                        )
                        require(
                          common.outputs.contains(
                            Node(coveringNode.value, Cons(cell.key, Some(key)))
                          ),
                          "Must correct node output"
                        )
                        require(
                          common.mint.flatten === List.single(
                            (
                              common.policy,
                              nodeToken(key),
                              BigInt(1)
                            )
                          ),
                          "Must mint correctly"
                        )
                    case _ => fail("Linked list must be empty")
            case _ => fail("Covering node input must be single")

    /** Remove a non-root node from the list.
      */
    def remove(
        common: Common,
        range: Interval,
        config: Config,
        outputs: List[TxOut],
        signatories: List[PubKeyHash],
        removeKey: PubKeyHash,
        cell: Cons
    ): Unit =
        val PubKeyHash(key) = removeKey
        require(
          cell.key.forall(_ < key) && cell.ref.forall(_ > key), // ???: logic???
          "Must cover remove key"
        )
        require( // INFO: parent cell to update with the `removeKey` ref & cell to remove with the key `removeKey`
          common.inputs.length == BigInt(2),
          "Must spend two nodes"
        )
        common.inputs.find(Cons(cell.key, Some(key)) === _.cell) match
            case Some(stayNode) =>
                require(
                  common.outputs.contains(
                    Node(stayNode.value, cell)
                  ),
                  "Must correct node output"
                )
                require(
                  common.mint.flatten === List.single(
                    (
                      common.policy,
                      nodeToken(key),
                      BigInt(-1)
                    )
                  ),
                  "Must mint correctly"
                )
                require(
                  signatories.contains(removeKey),
                  "Must sign by user"
                )
                // TODO: both find in a single pass
                common.inputs.find(cons(key, cell.ref) === _.cell) match
                    case Some(removeNode) =>
                        val fee = -Builtins.divideInteger(removeNode.value.getLovelace, -4)
                        require(
                          range.isEntirelyBefore(config.deadline) || outputs.exists(out =>
                              out.address === config.penalty && fee < out.value.getLovelace
                          ),
                          "Must satisfy removal broke phase rules"
                        )
                    case _ => fail("There must be a remove node input")
            case _ => fail("There must be a stay node input")

object LinkedListContract:

    inline def make(param: Config)(using scalus.Compiler.Options) =
        import scalus.builtin.ToData.toData
        compile(LinkedList.validate).toUplc().plutusV3 $ param.toData

    inline def compiled(using scalus.Compiler.Options) = compile(LinkedList.validate)

    def application: Application = Application
        .ofSingleValidator[Config, NodeAction](
          "LinkedList validator",
          "Linked list structures leverage the EUTXO model to enhancing scalability and throughput significantly. By linking multiple UTXOs together through a series of minting policies and validators, it can improve the user experience interacting with smart contract concurrently.",
          "1.0.0",
          LinkedList.validate
        )

    def blueprint: Blueprint = application.blueprint
