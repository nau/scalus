package scalus.sir.lowering

import org.scalatest.funsuite.AnyFunSuite
import scalus.*
import scalus.Compiler.compile
import scalus.builtin.*
import scalus.builtin.ByteString.hex
import scalus.builtin.Data.toData
import scalus.ledger.api.v3.*
import scalus.sir.*
import scalus.uplc.*
import scalus.uplc.eval.PlutusVM

class SirToUplcFromDataReplacementTest extends AnyFunSuite {

    given PlutusVM = PlutusVM.makePlutusV3VM()

    test("Import ScriptContext from Data") {
        val sir = compile { (scData: Data) =>
            val sc = Data.fromData[ScriptContext](scData)
            val tx = sc.txInfo
            tx.inputs.length
        }
        // println(sir.show)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        // println(term.show)

        val ownerPkh = PubKeyHash(hex"1234567890abcdef1234567890abcdef1234567890abcdef12345678")
        val scriptContext = makeSpendingScriptContext(
          datum = ownerPkh.toData,
          redeemer = "Hello, World!".toData,
          signatories = scalus.prelude.List(ownerPkh)
        )

        val termWithSc = term $ Term.Const(Constant.Data(Data.toData(scriptContext)))

        val l = termWithSc.evaluateDebug

        // println(l)
        pending
    }

    /*
    test("run Hello World") {
        import scalus.prelude.*
        val sir = compile { (scData: Data) =>
            val sc = Data.fromData[ScriptContext](scData)
            val tx = sc.txInfo
            sc.scriptInfo match {
                case ScriptInfo.SpendingScript(txOutRef, datum) =>
                    val Option.Some(ownerData) = datum: @unchecked
                    val owner = ownerData.to[PubKeyHash]
                    val signed = tx.signatories.contains(owner)
                    require(signed, "Must be signed")
                    val saysHello = sc.redeemer.to[String] == "Hello, World!"
                    require(saysHello, "Invalid redeemer")
            }
        }
        println(sir.showHighlighted)
        val lowering = SirToUplcV3Lowering(sir)
        val term = lowering.lower()
        println(term.showHighlighted)
        println(term.evaluateDebug)
    }
    
     */

    private def makeSpendingScriptContext(
        datum: Data,
        redeemer: Redeemer,
        signatories: scalus.prelude.List[PubKeyHash]
    ): ScriptContext = {
        import scalus.prelude.*
        val ownInput =
            TxInInfo(
              outRef = TxOutRef(
                id = TxId(randomByteString(32, 20)),
                idx = 0
              ),
              resolved = TxOut(
                address = Address(
                  Credential.ScriptCredential(randomByteString(28)),
                  Option.None
                ),
                value = Value.zero
              )
            )
        ScriptContext(
          txInfo = TxInfo(
            inputs = List(ownInput),
            fee = 188021,
            signatories = signatories,
            id = TxId(randomByteString(32, 20))
          ),
          redeemer = redeemer,
          scriptInfo = ScriptInfo.SpendingScript(
            txOutRef = ownInput.outRef,
            datum = Option.Some(datum)
          )
        )
    }

    def randomByteString(n: Int, seed: Int = 10): ByteString = {
        import scalus.builtin.ByteString
        val random = scala.util.Random(seed)
        ByteString(random.nextBytes(n)*)
    }

}
