package scalus.patterns

import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.*
import scalus.prelude.*
import org.scalatest.matchers.should.Matchers.*

class StakeValidatorTest extends StdlibTestKit with scalus.ledger.api.v3.ArbitraryInstances {
    // TODO: UPLC error with Data.unit
//    test("success spend") {
//        assertEvalSuccess {
//            val scriptHash = ByteString.empty
//            val credential = Credential.ScriptCredential(scriptHash)
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              withdrawals = SortedMap.singleton(credential, 0),
//              redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.spend(
//              withdrawalScriptHash = scriptHash,
//              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error
    ignore("failed spend with missing redeemer") {
        assertEvalFailsWithMessage[NoSuchElementException](StakeValidator.MissingRedeemer) {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.singleton(credential, 0),
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            StakeValidator.spend(
              withdrawalScriptHash = scriptHash,
              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error with Data.unit
//    test("failed spend with missing withdrawal") {
//        assertEvalFailsWithMessage[NoSuchElementException](StakeValidator.MissingWithdrawal) {
//            val scriptHash = ByteString.empty
//            val credential = Credential.ScriptCredential(scriptHash)
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              withdrawals = SortedMap.empty,
//              redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.spend(
//              withdrawalScriptHash = scriptHash,
//              withdrawalRedeemerValidator = (redeemer, lovelace) => true,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error with Data.unit
//    test("failed spend with withdrawal redeemer validator failed") {
//        assertEvalFailsWithMessage[RuntimeException](
//          StakeValidator.WithdrawalRedeemerValidatorFailed
//        ) {
//            val scriptHash = ByteString.empty
//            val credential = Credential.ScriptCredential(scriptHash)
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              withdrawals = SortedMap.singleton(credential, 0),
//              redeemers = SortedMap.singleton(ScriptPurpose.Rewarding(credential), Data.unit),
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.spend(
//              withdrawalScriptHash = scriptHash,
//              withdrawalRedeemerValidator = (redeemer, lovelace) => false,
//              txInfo = txInfo
//            )
//        }
//    }

    test("success spendMinimal") {
        assertEvalSuccess {
            val scriptHash = ByteString.empty
            val credential = Credential.ScriptCredential(scriptHash)

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.singleton(credential, 0),
              id = TxId(ByteString.empty)
            )

            StakeValidator.spendMinimal(
              withdrawalScriptHash = scriptHash,
              txInfo = txInfo
            )
        }
    }

    test("failed spendMinimal with missing withdrawal") {
        assertEvalFailsWithMessage[NoSuchElementException](StakeValidator.MissingWithdrawal) {
            val scriptHash = ByteString.empty

            val txInfo = TxInfo(
              inputs = List.empty,
              withdrawals = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            StakeValidator.spendMinimal(
              withdrawalScriptHash = scriptHash,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error with Data.unit
//    test("success withdraw") {
//        assertEvalSuccess {
//            val scriptHash = ByteString.empty
//            val credential = Credential.ScriptCredential(scriptHash)
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.withdraw(
//              withdrawalValidator = (redeemer, validatorHash, tx) => true,
//              redeemer = Data.unit,
//              credential = credential,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error with Data.unit
//    test("failed withdraw with pub key credential not supported") {
//        assertEvalFailsWithMessage[RuntimeException](StakeValidator.PubKeyCredentialNotSupported) {
//            val scriptHash = ByteString.empty
//            val credential = Credential.PubKeyCredential(PubKeyHash(scriptHash))
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.withdraw(
//              withdrawalValidator = (redeemer, validatorHash, tx) => true,
//              redeemer = Data.unit,
//              credential = credential,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error with Data.unit
//    test("failed withdraw with withdrawal validator failed") {
//        assertEvalFailsWithMessage[RuntimeException](StakeValidator.WithdrawalValidatorFailed) {
//            val scriptHash = ByteString.empty
//            val credential = Credential.ScriptCredential(scriptHash)
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              id = TxId(ByteString.empty)
//            )
//
//            StakeValidator.withdraw(
//              withdrawalValidator = (redeemer, validatorHash, tx) => false,
//              redeemer = Data.unit,
//              credential = credential,
//              txInfo = txInfo
//            )
//        }
//    }
}
