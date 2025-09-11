package scalus.examples

import scalus.*
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v1.PubKeyHash
import scalus.ledger.api.v1.Address
import scalus.ledger.api.v1.IntervalBoundType.*
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testkit.*
import scalus.prelude.Option.*

import scala.language.implicitConversions
import org.scalatest.funsuite.AnyFunSuite

class BettingTest extends AnyFunSuite, ScalusTest:
    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      // FIXME: SirToUplcV3Lowering
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplc110Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    test("Verify that a bet can be properly initialized"):
        val player1 = Mock.mockPubKeyHash(1)
        // Create test datum for a new bet
        val initialBetDatum = BetDatum(
          player1 = player1,
          // No second player yet
          player2 = PubKeyHash(ByteString.empty),
          oracle = Mock.mockPubKeyHash(3),
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = Mock.mockScriptHash(1)
        // Create test transaction that mints a bet token
        val testTransaction = TxInfo.placeholder.copy(
          outputs = List(
            TxOut(
              address = Address.fromScriptHash(policyId),
              // 3 ADA initial bet
              value = Value.lovelace(3_000_000) + Value(
                cs = policyId,
                tn = ByteString.fromString("lucky_number_slevin"),
                v = 1
              ),
              datum = OutputDatum.OutputDatum(initialBetDatum.toData)
            )
          ),
          signatories = List(player1),
          // 20th of July 2025 - for 5 minutes
          validRange = Interval(
            from = IntervalBound(Finite(1752989540), true),
            to = IntervalBound(Finite(1752990020), true),
          )
        )
        val result = BettingContract.compiled.runScript(
          ScriptContext(
            txInfo = testTransaction,
            scriptInfo = ScriptInfo.MintingScript(currencySymbol = policyId)
          )
        )
        assert(result.isSuccess, "Script execution should succeed for initial minting")

    test("Verify that player2 can join an existing bet"):
        val player1 = Mock.mockPubKeyHash(1)
        val player2 = Mock.mockPubKeyHash(2)
        val oracle = Mock.mockPubKeyHash(3)
        // Initial state: bet created by player1
        val initialBetDatum = BetDatum(
          player1,
          // No second player yet
          player2 = PubKeyHash(ByteString.empty),
          oracle = oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        // Updated state: player2 has joined
        val updatedBetDatum = BetDatum(
          player1,
          // No second player yet
          player2,
          oracle,
          // 31th of July 2025
          expiration = 1753939940,
        )
        val policyId = Mock.mockScriptHash(1)
        val tx = Mock.mockTxOutRef(1, 0)
        // Create test transaction where player2 joins
        val testTransaction = TxInfo.placeholder.copy(
          inputs = List(
            TxInInfo(
              outRef = tx,
              resolved = TxOut(
                address = Address.fromScriptHash(policyId),
                // Original 3 ADA bet
                value = Value.lovelace(3_000_000) + Value(
                  cs = policyId,
                  tn = ByteString.fromString("lucky_number_slevin"),
                  v = 1
                ),
                datum = OutputDatum.OutputDatum(initialBetDatum.toData)
              )
            )
          ),
          outputs = List(
            TxOut(
              address = Address.fromScriptHash(policyId),
              // Doubled to 6 ADA
              value = Value.lovelace(6_000_000) + Value(
                cs = policyId,
                tn = ByteString.fromString("lucky_number_slevin"),
                v = 1
              ),
              datum = OutputDatum.OutputDatum(updatedBetDatum.toData)
            )
          ),
          signatories = List(player2),
          // 22th of July 2025 - for 5 minutes
          validRange = Interval(
            from = IntervalBound(Finite(1753162820), true),
            to = IntervalBound(Finite(1753163120), true),
          )
        )
        val joinAction: Action = Action.Join
        val result = BettingContract.compiled.runScript(
          ScriptContext(
            txInfo = testTransaction,
            redeemer = joinAction.toData,
            scriptInfo = ScriptInfo.SpendingScript(
              txOutRef = tx,
              datum = Some(updatedBetDatum.toData)
            )
          )
        )
        assert(result.isSuccess, "Script execution should succeed for player2 joining spending")

    test("Verify that the oracle can announce winner and trigger payout"):
        assert(false)
