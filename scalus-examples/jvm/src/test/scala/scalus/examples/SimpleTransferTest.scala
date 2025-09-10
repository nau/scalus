package scalus.examples

import org.scalacheck.Gen
import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.Data
import scalus.builtin.Data.toData
import scalus.ledger.api.v2.OutputDatum
import scalus.ledger.api.v3.*
import scalus.prelude.*
import scalus.testkit.ScalusTest

class SimpleTransferTest extends AnyFunSuite with ScalusTest {
    val fee = 10
    inline given Compiler.Options = Compiler.Options(
      targetLoweringBackend = Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = true,
      // debug = false
    )

    private val sir = compile(SimpleTransfer.validate)

    private val hash: Gen[Hash] = genByteStringOfN(28)
    private val contract = hash.sample.get
    private val owner = hash.sample.get
    private val receiver = hash.sample.get

    private val datum = SimpleTransfer.Config(PubKeyHash(owner), PubKeyHash(receiver)).toData
    private val outputDatum = OutputDatum.OutputDatum(datum)
    private def deposit(amount: Value) = SimpleTransfer.Action.Deposit(amount).toData
    private def withdraw(amount: Value) = SimpleTransfer.Action.Withdraw(amount).toData

    test("deposit") {
        val ctx =
            context(
              Value.lovelace(0),
              deposit(Value.lovelace(1000)),
              List(PubKeyHash(owner)),
              List(makePubKeyHashInput(owner, BigInt(1000))),
              List(
                makeScriptHashOutput(contract, BigInt(1000), outputDatum)
              ),
            )
        val res = sir.runScript(ctx)
        assert(res.isSuccess, res.logs)
    }

    test("withdraw") {
        val ctx = context(
          Value.lovelace(1000),
          withdraw(Value.lovelace(500)),
          List(PubKeyHash(receiver)),
          outputs = List(
            makePubKeyHashOutput(receiver, BigInt(500 - fee)),
            makeScriptHashOutput(contract, BigInt(500), outputDatum)
          ),
        )
        val res = sir.runScript(ctx)
        assert(res.isSuccess, res.logs)
    }

    test("withdraw all") {
        val ctx = context(
          Value.lovelace(500),
          withdraw(Value.lovelace(500)),
          List(PubKeyHash(receiver)),
          outputs = List(
            makePubKeyHashOutput(receiver, BigInt(500 - fee))
          ),
        )
        val res = sir.runScript(ctx)
        assert(res.isSuccess, res.logs)
    }

    test("withdraw more") {
        val ctx = context(
          Value.lovelace(500),
          withdraw(Value.lovelace(1500)),
          List(PubKeyHash(receiver)),
          outputs = List(
            makePubKeyHashOutput(receiver, BigInt(1500 - fee))
          ),
        )
        val res = sir.runScript(ctx)
        assert(!res.isSuccess, res.logs)
    }

    private def context(
        balance: Value,
        redeemer: Data,
        signatories: List[PubKeyHash] = List.Nil,
        inputs: List[TxInInfo] = List.Nil,
        outputs: List[TxOut] = List.Nil,
    ): ScriptContext = {
        val ownInput =
            TxInInfo(
              outRef = random[TxOutRef],
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(contract),
                  Option.None
                ),
                value = balance
              )
            )
        ScriptContext(
          txInfo = TxInfo(
            inputs = inputs.prepended(ownInput),
            outputs = outputs,
            fee = fee,
            signatories = signatories,
            id = random[TxId]
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )

    }

}
