package scalus.patterns

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scalus.*
import scalus.builtin.Data
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testkit.ScalusTest

class TransactionLevelMinterValidatorSpec extends AnyFunSuite with ScalusTest {
    test("success spend") {
        val minterScriptHash = genByteStringOfN(28).sample.get
        val minterRedeemerValidator = (redeemer: Redeemer) => true
        val minterTokensValidator = (tokens: AssocMap[TokenName, BigInt]) => true

        val txInfo = TxInfo(
          inputs = List.empty,
          mint = AssocMap.singleton(minterScriptHash, AssocMap.empty),
          redeemers = AssocMap.singleton(ScriptPurpose.Minting(minterScriptHash), Data.unit),
          id = random[TxId]
        )

        noException should be thrownBy {
            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }
    }

    test("failed spend with missing redeemer") {
        val minterScriptHash = genByteStringOfN(28).sample.get
        val minterRedeemerValidator = (redeemer: Redeemer) => true
        val minterTokensValidator = (tokens: AssocMap[TokenName, BigInt]) => true

        val txInfo = TxInfo(
          inputs = List.empty,
          mint = AssocMap.singleton(minterScriptHash, AssocMap.empty),
          redeemers = AssocMap.empty,
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == TransactionLevelMinterValidator.MissingRedeemer)
    }

    test("failed spend with minter redeemer validator failed") {
        val minterScriptHash = genByteStringOfN(28).sample.get
        val minterRedeemerValidator = (redeemer: Redeemer) => false
        val minterTokensValidator = (tokens: AssocMap[TokenName, BigInt]) => true

        val txInfo = TxInfo(
          inputs = List.empty,
          mint = AssocMap.singleton(minterScriptHash, AssocMap.empty),
          redeemers = AssocMap.singleton(ScriptPurpose.Minting(minterScriptHash), Data.unit),
          id = random[TxId]
        )

        val exception = intercept[RuntimeException] {
            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }

        assert(
          exception.getMessage == TransactionLevelMinterValidator.MinterRedeemerValidatorFailed
        )
    }

    test("failed spend with minter tokens validator failed") {
        val minterScriptHash = genByteStringOfN(28).sample.get
        val minterRedeemerValidator = (redeemer: Redeemer) => true
        val minterTokensValidator = (tokens: AssocMap[TokenName, BigInt]) => false

        val txInfo = TxInfo(
          inputs = List.empty,
          mint = AssocMap.singleton(minterScriptHash, AssocMap.empty),
          redeemers = AssocMap.singleton(ScriptPurpose.Minting(minterScriptHash), Data.unit),
          id = random[TxId]
        )

        val exception = intercept[RuntimeException] {
            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == TransactionLevelMinterValidator.MinterTokensValidatorFailed)
    }

    test("success spendMinimal") {
        val minterScriptHash = genByteStringOfN(28).sample.get

        val txInfo = TxInfo(
          inputs = List.empty,
          mint = AssocMap.singleton(minterScriptHash, AssocMap.empty),
          redeemers = AssocMap.empty,
          id = random[TxId]
        )

        noException should be thrownBy {
            TransactionLevelMinterValidator.spendMinimal(
              minterScriptHash = minterScriptHash,
              txInfo = txInfo
            )
        }
    }

    test("failed spendMinimal with missing mint") {
        val minterScriptHash = genByteStringOfN(28).sample.get

        val txInfo = TxInfo(
          inputs = List.empty,
          mint = AssocMap.empty,
          redeemers = AssocMap.empty,
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            TransactionLevelMinterValidator.spendMinimal(
              minterScriptHash = minterScriptHash,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == TransactionLevelMinterValidator.MissingMint)
    }
}
