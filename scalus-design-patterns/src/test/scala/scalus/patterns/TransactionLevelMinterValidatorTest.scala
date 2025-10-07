package scalus.patterns

import org.scalatest.matchers.should.Matchers.*
import scalus.*
import scalus.builtin.{ByteString, Data}
import scalus.ledger.api.v3.*
import scalus.prelude.*

class TransactionLevelMinterValidatorTest
    extends StdlibTestKit
    with scalus.ledger.api.v3.ArbitraryInstances {

    // TODO: UPLC error with Data.unit
//    test("success spend") {
//        assertEvalSuccess {
//            val minterScriptHash = ByteString.empty
//            val minterRedeemerValidator = (redeemer: Redeemer) => true
//            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => true
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              mint = Value.zero,
//              redeemers = SortedMap.singleton(ScriptPurpose.Minting(minterScriptHash), Data.unit),
//              id = TxId(ByteString.empty)
//            )
//
//            TransactionLevelMinterValidator.spend(
//              minterScriptHash = minterScriptHash,
//              minterRedeemerValidator = minterRedeemerValidator,
//              minterTokensValidator = minterTokensValidator,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error
    ignore("failed spend with missing redeemer") {
        assertEvalFailsWithMessage[NoSuchElementException](
          TransactionLevelMinterValidator.MissingRedeemer
        ) {
            val minterScriptHash = ByteString.empty
            val minterRedeemerValidator = (redeemer: Redeemer) => true
            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => true

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spend(
              minterScriptHash = minterScriptHash,
              minterRedeemerValidator = minterRedeemerValidator,
              minterTokensValidator = minterTokensValidator,
              txInfo = txInfo
            )
        }
    }

    // TODO: UPLC error
//    test("failed spend with minter redeemer validator failed") {
//        assertEvalFailsWithMessage[RuntimeException](
//          TransactionLevelMinterValidator.MinterRedeemerValidatorFailed
//        ) {
//            val minterScriptHash = ByteString.empty
//            val minterRedeemerValidator = (redeemer: Redeemer) => false
//            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => true
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              mint = Value.zero,
//              redeemers = SortedMap.singleton(ScriptPurpose.Minting(minterScriptHash), Data.unit),
//              id = TxId(ByteString.empty)
//            )
//
//            TransactionLevelMinterValidator.spend(
//              minterScriptHash = minterScriptHash,
//              minterRedeemerValidator = minterRedeemerValidator,
//              minterTokensValidator = minterTokensValidator,
//              txInfo = txInfo
//            )
//        }
//    }

    // TODO: UPLC error
//    test("failed spend with minter tokens validator failed") {
//        assertEvalFailsWithMessage[RuntimeException](
//          TransactionLevelMinterValidator.MinterTokensValidatorFailed
//        ) {
//            val minterScriptHash = ByteString.empty
//            val minterRedeemerValidator = (redeemer: Redeemer) => true
//            val minterTokensValidator = (tokens: SortedMap[TokenName, BigInt]) => false
//
//            val txInfo = TxInfo(
//              inputs = List.empty,
//              mint = Value.zero,
//              redeemers = SortedMap.singleton(ScriptPurpose.Minting(minterScriptHash), Data.unit),
//              id = TxId(ByteString.empty)
//            )
//
//            TransactionLevelMinterValidator.spend(
//              minterScriptHash = minterScriptHash,
//              minterRedeemerValidator = minterRedeemerValidator,
//              minterTokensValidator = minterTokensValidator,
//              txInfo = txInfo
//            )
//        }
//    }

    test("success spendMinimal") {
        assertEvalSuccess {
            val minterScriptHash = ByteString.empty

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value(minterScriptHash, ByteString.empty, BigInt(1)),
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spendMinimal(
              minterScriptHash = minterScriptHash,
              txInfo = txInfo
            )
        }
    }

    test("failed spendMinimal with missing mint") {
        assertEvalFailsWithMessage[NoSuchElementException](
          TransactionLevelMinterValidator.MissingMint
        ) {
            val minterScriptHash = ByteString.empty

            val txInfo = TxInfo(
              inputs = List.empty,
              mint = Value.zero,
              redeemers = SortedMap.empty,
              id = TxId(ByteString.empty)
            )

            TransactionLevelMinterValidator.spendMinimal(
              minterScriptHash = minterScriptHash,
              txInfo = txInfo
            )
        }
    }
}
