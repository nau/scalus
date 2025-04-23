package scalus.examples

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.{*, given}
import scalus.prelude.{*, given}
import scalus.testkit.ScalusTest

class StakeValidatorSpec extends AnyFunSuite with ScalusTest {
    test("success spend") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = AssocMap.singleton(credential, 0),
          redeemers = AssocMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
          id = random[TxId]
        )

        assert(
          StakeValidator.spend(
            scriptHash,
            (redeemer, lovelace) => true,
            txInfo
          )
        )
    }

    test("spend missing redeemer") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = AssocMap.singleton(credential, 0),
          redeemers = AssocMap.empty,
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            StakeValidator.spend(
              scriptHash,
              (redeemer, lovelace) => true,
              txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.MissingRedeemer)
    }

    test("spend missing withdrawal") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = AssocMap.empty,
          redeemers = AssocMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
          id = random[TxId]
        )

        val exception = intercept[NoSuchElementException] {
            StakeValidator.spend(
              scriptHash,
              (redeemer, lovelace) => true,
              txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.MissingWithdrawal)
    }

    test("success spendMinimal") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = AssocMap.singleton(credential, 0),
          id = random[TxId]
        )

        assert(
          StakeValidator.spendMinimal(scriptHash, txInfo)
        )
    }

    test("fail spendMinimal") {
        val scriptHash = genByteStringOfN(28).sample.get

        val txInfo = TxInfo(
          inputs = List.empty,
          withdrawals = AssocMap.empty,
          id = random[TxId]
        )

        assert(
          !StakeValidator.spendMinimal(scriptHash, txInfo)
        )
    }

    test("success withdraw") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.ScriptCredential(scriptHash)

        val txInfo = TxInfo(
          inputs = List.empty,
          id = random[TxId]
        )

        assert(
          StakeValidator.withdraw(
            (redeemer, validatorHash, tx) => true,
            Data.unit,
            credential,
            txInfo
          )
        )
    }

    test("fail withdraw") {
        val scriptHash = genByteStringOfN(28).sample.get
        val credential = Credential.PubKeyCredential(PubKeyHash(scriptHash))

        val txInfo = TxInfo(
          inputs = List.empty,
          id = random[TxId]
        )

        val exception = intercept[RuntimeException] {
            StakeValidator.withdraw(
              (redeemer, validatorHash, tx) => true,
              Data.unit,
              credential,
              txInfo
            )
        }

        assert(exception.getMessage == StakeValidator.PubKeyCredentialNotSupported)
    }
}
