package scalus.bloxbean

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.api.util.CostModelUtil
import com.bloxbean.cardano.client.common.model.Networks
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.ByteString.*
import scalus.builtin.{platform, ByteString, Data}
import scalus.cardano.address.ShelleyDelegationPart.Null
import scalus.cardano.address.{Address, Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.examples.PubKeyValidator
import scalus.uplc.*
import scalus.uplc.eval.ExBudget

class NewTxEvaluatorTest extends AnyFunSuite:
    val senderMnemonic: String =
        "drive useless envelope shine range ability time copper alarm museum near flee wrist live type device meadow allow churn purity wisdom praise drop code"
    val sender1 = new Account(Networks.testnet(), senderMnemonic)
    val sender1Addr: String = sender1.baseAddress()

    test("TxEvaluator PlutusV2") {
        val costMdls = com.bloxbean.cardano.client.plutus.spec.CostMdls()
        costMdls.add(CostModelUtil.PlutusV1CostModel)
        costMdls.add(CostModelUtil.PlutusV2CostModel)
        val evaluator = NewTxEvaluator(
          SlotConfig.Mainnet,
          initialBudget = ExBudget.fromCpuAndMemory(10_000000000L, 10_000000L),
          protocolMajorVersion = 10,
          costMdls = costMdls
        )
        inline def requiredPubKeyHash =
            hex"e1ac75d278929abc5e113cd1cd611a35af2520e7b3056ecac3da186b"
        val pubKeyValidator =
            compile(PubKeyValidator.validatorV2(requiredPubKeyHash))
                .toUplc()
                .plutusV2
        val s = Script.PlutusV2(ByteString.unsafeFromArray(pubKeyValidator.cborEncoded))
        val input = TransactionInput(Hash(platform.blake2b_256(ByteString.fromString("asdf"))), 0)
        val datum = Data.unit
        val dataHash: DataHash = Hash(platform.blake2b_256(datum.toCborByteString))
        val addr =
            "addr1qxwg0u9fpl8dac9rkramkcgzerjsfdlqgkw0q8hy5vwk8tzk5pgcmdpe5jeh92guy4mke4zdmagv228nucldzxv95clqe35r3m"
        val utxo = Map(
          input -> TransactionOutput(
            address = Address.Shelley(
              ShelleyAddress(
                Network.Mainnet,
                payment = ShelleyPaymentPart.Script(s.scriptHash),
                delegation = Null
              ),
            ),
            datumOption = Some(DatumOption.Hash(dataHash)),
            value = Value.lovelace(2)
          )
        )
        val redeemer = Redeemer(RedeemerTag.Spend, 0, Data.unit, ExUnits(0, 0))
        val tx = Transaction(
          KeepRaw(
            TransactionBody(
              inputs = Set(input),
              outputs = Vector(
                Sized(
                  TransactionOutput(
                    address = Address.fromByteString(AddressBytes.fromBech32(addr)),
                    value = Value.lovelace(2)
                  )
                )
              ),
              fee = Coin(0),
              requiredSigners = Set(Hash(requiredPubKeyHash)),
            )
          ),
          witnessSet = TransactionWitnessSet(
            redeemers = Some(KeepRaw(Redeemers.Array(Vector(redeemer)))),
            plutusV2Scripts = Set(s),
            plutusData = KeepRaw(TaggedSet(Set(KeepRaw(datum)))),
          ),
          isValid = true
        )
        val redeemers = evaluator.evalPhaseTwo(tx, utxo)
        assert(redeemers.size == 1)
        val redeemerResult = redeemers.head
        assert(redeemerResult.exUnits.memory == 13375L)
        assert(redeemerResult.exUnits.steps == 3732764L)
    }
