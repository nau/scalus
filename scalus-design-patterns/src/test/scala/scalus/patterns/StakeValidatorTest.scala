package scalus.patterns

import scalus.*
import scalus.builtin.Data
import scalus.ledger.api.v3.*
import scalus.prelude.*
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.*
import scalus.testkit.ScalusTest

class StakeValidatorTest extends AnyFunSuite with ScalusTest {
    test("success spend") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = SortedMap.singleton(credential, 0),
          redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
          id = random[TxId]
        )

        noException should be thrownBy {
            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }
    }

    test("failed spend with missing redeemer") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = SortedMap.singleton(credential, 0),
          redeemers = SortedMap.empty,
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.MissingRedeemer)
    }

    test("failed spend with missing withdrawal") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = SortedMap.empty,
          redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.MissingWithdrawal)
    }

    test("failed spend with withdrawal redeemer validator failed") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = SortedMap.singleton(credential, 0),
          redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
          id = random[TxId]
        )

        val exception = intercept[RuntimeException] {
            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => false,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.WithdrawalRedeemerValidatorFailed)
    }

    test("success spendMinimal") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = SortedMap.singleton(credential, 0),
          id = random[TxId]
        )

        noException should be thrownBy {
            StakeValidator.spendMinimal(
              withdrawalScriptHash = scriptHash,
              txInfo = txInfo
            )
        }
    }

    test("failed spendMinimal with missing withdrawal") {
        val scriptHash = genByteStringOfN(28).sample.get

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = SortedMap.empty,
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            StakeValidator.spendMinimal(
              withdrawalScriptHash = scriptHash,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.MissingWithdrawal)
    }

    test("success withdraw") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          id = random[TxId]
        )

        noException should be thrownBy {
            StakeValidator.withdraw(
              withdrawalValidator = (redeemer, validatorHash, tx) => true,
              redeemer = Data.unit,
              credential = credential,
              txInfo = txInfo
            )
        }
    }

    test("failed withdraw with pub key credential not supported") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.PubKeyCredential(PubKeyHash(scriptHash))

        val txInfo = TxInfo(
          inputs = List.empty,
          id = random[TxId]
        )

        val exception = intercept[RuntimeException] {
            StakeValidator.withdraw(
              withdrawalValidator = (redeemer, validatorHash, tx) => true,
              redeemer = Data.unit,
              credential = credential,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.PubKeyCredentialNotSupported)
    }

    test("failed withdraw with withdrawal validator failed") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          id = random[TxId]
        )

        val exception = intercept[RuntimeException] {
            StakeValidator.withdraw(
              withdrawalValidator = (redeemer, validatorHash, tx) => false,
              redeemer = Data.unit,
              credential = credential,
              txInfo = txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.WithdrawalValidatorFailed)
    }
}
