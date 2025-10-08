package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scalus.*
import scalus.builtin.ByteString.*
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.ledger.api.v3.TxId.*
import scalus.prelude.*
import scalus.prelude.Option.*
import scalus.testkit.ScalusTest

import scala.language.implicitConversions

class LinkedListTest extends AnyFunSuite, ScalusTest:
    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      debug = false
    )

    val policyId = hex"746fa3ba2daded6ab9ccc1e39d3835aa1dfcb9b5a54acc2ebe6b79a4"
    val txId = txid"2c6dbc95c1e96349c4131a9d19b029362542b31ffd2340ea85dd8f28e271ff6d"
    val initOutputRef = TxOutRef(id = txId, idx = 1)
    val config = Config(
      init = initOutputRef,
      deadline = 86_400_000L,
      penalty = Address.fromScriptHash(utf8"P")
    )
    val user1 = hex"a65ca58a4e9c755fa830173d2a5caed458ac0c73f97db7faae2e7e3b"
    val user2 = hex"e18d73505be6420225ed2a42c8e975e4c6f9148ab38e951ea2572e54"

    def key(token: TokenName): TokenName = LinkedList.nodeToken ++ token

    test("Verify that a linked list can be properly initialized"):
        val mintedValue = Value(cs = policyId, tn = LinkedList.nodeToken, v = 1)
        val headOutput = TxOut(
          address = Address.fromScriptHash(utf8"B"),
          value = Value.lovelace(4_000_000) + mintedValue,
          datum = OutputDatum.OutputDatum(SetNode(key = None, link = None).toData)
        )
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = initOutputRef,
              resolved = TxOut(
                address = Address.fromScriptHash(utf8"C"),
                value = Value.lovelace(4_000_000),
                datum = OutputDatum.NoOutputDatum
              )
            )
          ),
          outputs = List.single(headOutput),
          mint = mintedValue,
          id = txId
        )
        val result = LinkedListContract.compiled.runScript(
          scriptContext = ScriptContext(
            txInfo = tx,
            redeemer = NodeAction.Init.toData,
            scriptInfo = ScriptInfo.MintingScript(policyId)
          ),
          param = Some(config.toData)
        )
        if result.isFailure then result.logs.foreach(println)
        assert(result.isSuccess, "Linked list initialization should succeed")

    test("Verify that a linked list can be properly de-initialized (burn)"):
        val burnValue = Value(cs = policyId, tn = LinkedList.nodeToken, v = -1)
        val nodeOut = TxOut(
          address = Address.fromScriptHash(utf8"B"),
          value = Value.lovelace(4_000_000) + burnValue,
          datum = OutputDatum.OutputDatum(SetNode(key = None, link = None).toData)
        )
        val tx = TxInfo.placeholder.copy(
          inputs = List(TxInInfo(initOutputRef, nodeOut)),
          mint = burnValue,
          id = txId
        )
        val result = LinkedListContract.compiled.runScript(
          scriptContext = ScriptContext(
            txInfo = tx,
            redeemer = NodeAction.Deinit.toData,
            scriptInfo = ScriptInfo.MintingScript(policyId)
          ),
          param = Some(config.toData)
        )
        if result.isFailure then result.logs.foreach(println)
        assert(result.isSuccess, "Linked list de-initialization should succeed")

    test("Verify that de-initialization fails if the list is not empty"):
        val nonEmptyNode = SetNode(
          key = Some(user1),
          link = Some(user2)
        )
        val nodeValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = -1)
        val nodeOut = TxOut(
          address = Address.fromScriptHash(utf8"I"),
          value = nodeValue,
          datum = OutputDatum.OutputDatum(nonEmptyNode.toData)
        )
        val burnValue = Value(cs = policyId, tn = LinkedList.nodeToken, v = -1)
        val tx = TxInfo.placeholder.copy(
          inputs = List(TxInInfo(initOutputRef, nodeOut)),
          mint = burnValue,
          id = txId
        )
        val result = LinkedListContract.compiled.runScript(
          scriptContext = ScriptContext(
            txInfo = tx,
            redeemer = NodeAction.Deinit.toData,
            scriptInfo = ScriptInfo.MintingScript(policyId)
          ),
          param = Some(config.toData)
        )
        if result.isSuccess then result.logs.foreach(println)
        assert(result.isFailure, "De-initialization should fail if the list is not empty")

    test("Verify that a new node can be inserted into the linked list"):
        val coveringNode = SetNode(key = Some(user1), link = None)
        val coveringValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val coveringOutput = TxOut(
          Address.fromScriptHash(utf8"I"),
          coveringValue,
          OutputDatum.OutputDatum(coveringNode.toData)
        )
        val coveringRef = TxOutRef(id = txid"", idx = 1)
        val newNode = SetNode(key = Some(user2), link = None)
        val insertValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val newNodeOutput = TxOut(
          Address.fromScriptHash(utf8"I"),
          insertValue,
          OutputDatum.OutputDatum(newNode.toData)
        )
        val tx = TxInfo.placeholder.copy(
          inputs = List.single(TxInInfo(coveringRef, coveringOutput)),
          outputs = List(
            coveringOutput.copy(datum =
                OutputDatum.OutputDatum(SetNode(key = Some(user1), link = Some(user2)).toData)
            ),
            newNodeOutput
          ),
          mint = Value(cs = policyId, tn = key(user2), v = 1),
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txId
        )
        val result = LinkedListContract.compiled.runScript(
          scriptContext = ScriptContext(
            txInfo = tx,
            redeemer = NodeAction.Insert(PubKeyHash(user2), coveringNode).toData,
            scriptInfo = ScriptInfo.MintingScript(policyId)
          ),
          param = Some(config.toData)
        )
        if result.isFailure then result.logs.foreach(println)
        assert(result.isSuccess, "Linked list insertion should succeed")

    test("Verify that a node can be removed from the linked list"):
        val coveringNode = SetNode(key = Some(user1), link = Some(user2))
        val removeNode = SetNode(key = Some(user2), link = None)
        val coveringValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user1), v = 1)
        val removeValue = Value.lovelace(9_000_000) + Value(cs = policyId, tn = key(user2), v = 1)
        val coveringOutput = TxOut(
          Address.fromScriptHash(utf8"I"),
          coveringValue,
          OutputDatum.OutputDatum(coveringNode.toData)
        )
        val removeOutput = TxOut(
          Address.fromScriptHash(utf8"I"),
          removeValue,
          OutputDatum.OutputDatum(removeNode.toData)
        )
        val coveringRef = TxOutRef(id = txid"", idx = 1)
        val removeRef = TxOutRef(id = txid"", idx = 1)
        val updatedNode = SetNode(key = Some(user1), link = None)
        val updatedOutput = TxOut(
          Address.fromScriptHash(utf8"I"),
          coveringValue,
          OutputDatum.OutputDatum(updatedNode.toData)
        )
        val burnValue = Value(cs = policyId, tn = key(user2), v = -1)
        val tx = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(removeRef, removeOutput),
            TxInInfo(coveringRef, coveringOutput)
          ),
          outputs = List.single(updatedOutput),
          mint = burnValue,
          validRange = Interval.entirelyBetween(1000L, 2000L),
          signatories = List.single(PubKeyHash(user2)),
          id = txId
        )
        val result = LinkedListContract.compiled.runScript(
          scriptContext = ScriptContext(
            txInfo = tx,
            redeemer = NodeAction
                .Remove(PubKeyHash(user2), SetNode(key = Some(user1), link = None))
                .toData,
            scriptInfo = ScriptInfo.MintingScript(policyId)
          ),
          param = Some(config.toData)
        )
        if result.isFailure then result.logs.foreach(println)
        assert(result.isSuccess, "Linked list removal should succeed")
