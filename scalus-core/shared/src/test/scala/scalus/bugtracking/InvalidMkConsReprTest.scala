package scalus.bugtracking

import org.scalatest.funsuite.AnyFunSuite
import scalus.Compiler.compile
import scalus.*
import scalus.prelude.{*, given}
import scalus.builtin.{ByteString, Data}
import scalus.builtin.Data.toData
import scalus.ledger.api.v1.Credential.{PubKeyCredential, ScriptCredential}
import scalus.ledger.api.v1.{Credential, PubKeyHash, ScriptHash, Value}
import scalus.ledger.api.v1.Value.getLovelace
import scalus.ledger.api.v2.TxOut
import scalus.ledger.api.v3.*
import scalus.uplc.eval.*

@Compile
object Min20250712 extends DataParameterizedValidator {

    override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {

        val payees = payeesData
            .to[List[ByteString]]
            .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

        val myTxInputCredential =
            tx.inputs.find(_.outRef === ownRef).get.resolved.address.credential

        // Find the first and single payee that triggers the payout and pays the fee
        //  and calculate the sum of contract inputs
        val (optPayeeInputWithChange, sumContractInputs) = tx.inputs
            .foldLeft(Option.empty[TxOut], BigInt(0)) {
                case ((optTxOut, sumContractInputs), input) =>
                    if payees.contains(input.resolved.address.credential)
                    then
                        if optTxOut.isEmpty then (Option.Some(input.resolved), sumContractInputs)
                        else fail("Already found a fee payer")
                    else if input.resolved.address.credential === myTxInputCredential then
                        (optTxOut, sumContractInputs + input.resolved.value.getLovelace)
                    else
                        // TODO: think
                        fail("Input not from the contract or payer")
            }

        val payeeInputWithChange = optPayeeInputWithChange.getOrFail(
          "One of the payees must have an input to pay the fee and trigger the payout"
        )

        val (sumOutput, sumsPerPayee) =
            tx.outputs.foldLeft(
              (BigInt(0), AssocMap.empty[Credential.PubKeyCredential, BigInt])
            ) { case (state, output) =>
                val (sum, sumsPerPayee) = state
                val value = output.value.getLovelace
                val payee: Credential.PubKeyCredential = output.address.credential match
                    case Credential.PubKeyCredential(pkh) => Credential.PubKeyCredential(pkh)
                    case _                                => fail("Output to script is not allowed")
                sumsPerPayee.get(payee) match
                    case Option.None => (sum + value, sumsPerPayee.insert(payee, value))
                    case Option.Some(prevSum) =>
                        (sum + value, sumsPerPayee.insert(payee, prevSum + value))
            }

        val (optSplit, optPayeeSumWithChange, nPayed) =
            sumsPerPayee.toList.foldLeft(
              (Option.empty[BigInt], Option.empty[BigInt], BigInt(0))
            ) { case ((optSplit, optPayeeSumWithChange, nPayed), (payee, value)) =>
                require(payees.contains(payee), "Must pay to a payee")
                if payeeInputWithChange.address.credential === payee
                then (optSplit, Option.Some(value), nPayed + 1)
                else
                    optSplit match
                        case Option.None => (Option.Some(value), optPayeeSumWithChange, nPayed + 1)
                        case Option.Some(split) =>
                            require(split === value, "Split unequally")
                            (Option.Some(split), optPayeeSumWithChange, nPayed + 1)
            }

        require(payees.length == nPayed, "Not all payees were paid")
        optSplit match
            case Option.None => // one payee, no split
            case Option.Some(split) =>
                val payeeSumWithChange = optPayeeSumWithChange.getOrFail("No change output")
                val eqSumValue = sumOutput - payeeSumWithChange + split
                val reminder = sumContractInputs - eqSumValue
                require(reminder < nPayed, "value to be payed to payees is too low")
        //    nOutputs * (split + 1) > sumContractInputs   <=>
        //    nOutputs * split + nOutputs > sumContractInputs <=>
        //    eqSumValue + nOutputs > sumContractInputs <=>
        //    nOutputs > reminder ( = sumContractInputs - eqSumValue)
        //
        // max number of payers ≈ 250 (16kB / 28 bytes / 2 (inputs and outputs))
        // thus, up to 250 lovelace of reminder is possible, so we can ignore it

    }

}

@Compile
object Min20250712_1 extends DataParameterizedValidator {

    override def spend(
        payeesData: Data,
        datum: Option[Data],
        redeemer: Data,
        tx: TxInfo,
        ownRef: TxOutRef
    ): Unit = {

        val payees: List[Credential.PubKeyCredential] = payeesData
            .to[List[ByteString]]
            .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))
        val aPayee: Credential.PubKeyCredential = payees.head

        // val myTxInputCredential =
        //    tx.inputs.find(_.outRef === ownRef).get.resolved.address.credential

        // Find the first and single payee that triggers the payout and pays the fee
        //  and calculate the sum of contract inputs
        val (payeeInputWithChange, sumContractInputs) =
            (tx.inputs.head.resolved, BigInt(30))

        val (sumOutput, sumsPerPayee) = (
          BigInt(38),
          AssocMap.empty[Credential.PubKeyCredential, BigInt].insert(aPayee, BigInt(38))
        )

        /*
        val (optSplit, optPayeeSumWithChange, nPayed) =
            sumsPerPayee.toList.foldLeft(
              (Option.empty[BigInt], Option.empty[BigInt], BigInt(0))
            ) { case ((optSplit, optPayeeSumWithChange, nPayed), (payee, value)) =>
                (Option.Some(value), optPayeeSumWithChange, nPayed + 1)
            }
            
         */

        val (optSplit, optPayeeSumWithChange, nPayed) =
            sumsPerPayee.toList.foldLeft(
              (Option.empty[BigInt], Option.empty[BigInt], BigInt(0))
            ) { case ((optSplit, optPayeeSumWithChange, nPayed), (payee, value)) =>
                (Option.Some(value), optPayeeSumWithChange, nPayed + 1)
            }

    }

}

class InvalidMkConsReprTest extends AnyFunSuite {

    inline given scalus.Compiler.Options = scalus.Compiler.Options(
      targetLoweringBackend = scalus.Compiler.TargetLoweringBackend.SirToUplcV3Lowering,
      generateErrorTraces = true,
      optimizeUplc = false,
      debug = false
    )

    given PlutusVM = PlutusVM.makePlutusV3VM()

    val pkhA = PubKeyHash(ByteString.fromString("A" * 32))
    val pkhB = PubKeyHash(ByteString.fromString("B" * 32))
    val scriptHash = ByteString.fromString("scriptHash")

    test("readingbytes string should compile and run") {
        // pending
        val sir = compile { (d: Data) =>
            // val payes = d
            //    .to[List[ByteString]]
            //    .map(payee => Credential.PubKeyCredential(PubKeyHash(payee)))

            // val payes = d
            //    .to[List[ByteString]]
            //    .map(payee => PubKeyHash(payee))

            val payes = d
                .to[List[ByteString]]
                .map(payee => payee)

            // def process(x: List[ByteString]): List[Credential.PubKeyCredential] = {
            //    x match {
            //        case List.Cons(head, tail) =>
            //            List.Cons(Credential.PubKeyCredential(PubKeyHash(head)), process(tail))
            //        case List.Nil =>
            //            List.empty[Credential.PubKeyCredential]
            //    }
            //
            // }

            // val u = process(d.to[List[ByteString]])

        }

        println(sir.pretty.render(100))
        val uplc = sir.toUplc(generateErrorTraces = true)

        println(uplc.pretty.render(100))

        val payeesData = List(pkhA, pkhB).toData

        val applied = uplc.plutusV3 $ payeesData

        // val programWithContext = applied $ context.toData

        val result = applied.evaluateDebug
        println(s"result=$result")
        assert(result.isSuccess)
    }

    test("Unconstr service context in fold should compile and run") {

        val sir = compile { (txInputsData: Data) =>
            val txInputs = txInputsData.to[List[TxInInfo]]

            val (optPayeeInputWithChange, sumContractInputs) = txInputs
                .foldLeft(Option.empty[TxOut], BigInt(0)) { case (acc, input) =>
                    (acc._1, acc._2 + input.resolved.value.getLovelace)
                }

        }

        val inTxId = TxId(ByteString.fromString("inTxId"))
        val lockTxId = TxId(ByteString.fromString("lockTxId"))
        val currentTxId = TxId(ByteString.fromString("currentTxId"))

        val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkhA), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = scalus.prelude.List(
              TxInInfo(
                outRef = TxOutRef(inTxId, 0),
                resolved = TxOut(
                  address = Address(PubKeyCredential(pkhA), Option.None),
                  value = Value.lovelace(10)
                )
              ),
              TxInInfo(
                outRef = TxOutRef(lockTxId, 0),
                resolved = TxOut(
                  address = Address(ScriptCredential(scriptHash), Option.None),
                  value = Value.lovelace(30)
                )
              )
            ),
            outputs = List(
              TxOut(
                address = Address(
                  PubKeyCredential(pkhA),
                  Option.None
                ),
                value = Value.lovelace(38)
              )
            ),
            fee = 2,
            certificates = scalus.prelude.List(txCert),
            signatories = scalus.prelude.List(pkhA),
            redeemers = AssocMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), Data.unit))
            ),
            id = currentTxId
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef = txOutRef),
        )

        val pl = List(pkhA).toData

        val uplc = sir.toUplc(generateErrorTraces = true)
        println(uplc.pretty.render(100))

        val applied = uplc.plutusV3 $ context.txInfo.inputs.toData

        val result = applied.evaluateDebug

        assert(result.isSuccess, s"Result: $result")

    }

    test("reading of input value") {

        val sir = compile { (txInInfoData: Data) =>
            val txInInfo = txInInfoData.to[TxInInfo]

            val lv = txInInfo.resolved.value.getLovelace

            require(lv == BigInt(10), "Expected input value to be 10")

        }

        val inTxId = TxId(ByteString.fromString("inTxId"))
        val lockTxId = TxId(ByteString.fromString("lockTxId"))
        val currentTxId = TxId(ByteString.fromString("currentTxId"))

        val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkhA), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = scalus.prelude.List(
              TxInInfo(
                outRef = TxOutRef(inTxId, 0),
                resolved = TxOut(
                  address = Address(PubKeyCredential(pkhA), Option.None),
                  value = Value.lovelace(10)
                )
              ),
              TxInInfo(
                outRef = TxOutRef(lockTxId, 0),
                resolved = TxOut(
                  address = Address(ScriptCredential(scriptHash), Option.None),
                  value = Value.lovelace(30)
                )
              )
            ),
            outputs = List(
              TxOut(
                address = Address(
                  PubKeyCredential(pkhA),
                  Option.None
                ),
                value = Value.lovelace(38)
              )
            ),
            fee = 2,
            certificates = scalus.prelude.List(txCert),
            signatories = scalus.prelude.List(pkhA),
            redeemers = AssocMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), Data.unit))
            ),
            id = currentTxId
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef = txOutRef),
        )

        val pl = List(pkhA).toData

        val lw = sir.toLoweredValue(generateErrorTraces = true)
        println(lw.pretty.render(100))

        val uplc = sir.toUplc(generateErrorTraces = true)
        println(uplc.pretty.render(100))

        val input = context.txInfo.inputs.head
        println(s"input=${input}")
        println(s"lovelace: ${input.resolved.value.getLovelace}")

        val applied = uplc.plutusV3 $ context.txInfo.inputs.head.toData

        val result = applied.evaluateDebug

        println(s"evaluation result: ${result}")

        assert(result.isSuccess, s"Result: $result")

    }

    test("check foldLeft over assocmap[ByteString, BigInt]") {

        val sir = compile {

            val aPayee: ByteString = ByteString.fromString("AAAA")

            val sumsPerPayee =
                AssocMap.empty[ByteString, BigInt].insert(aPayee, BigInt(38))

        }

        val lw = sir.toLoweredValue(generateErrorTraces = true)
        println(lw.pretty.render(100))

        val uplc = sir.toUplc(generateErrorTraces = true)
        println(uplc.pretty.render(100))

        val result = uplc.evaluateDebug

        assert(result.isSuccess, s"Result: $result")

    }

    test("check sericalization with unB ") {

        val sir = compile {

            val aPayee: ByteString = ByteString.fromString("AAAA")

            val sumsPerPayee =
                AssocMap.empty[ByteString, BigInt].insert(aPayee, BigInt(38))

            val (optSplit, optPayeeSumWithChange, nPayed) =
                sumsPerPayee.toList.foldLeft(
                  (Option.empty[BigInt], Option.empty[BigInt], BigInt(0))
                ) { case ((optSplit, optPayeeSumWithChange, nPayed), (payee, value)) =>
                    (Option.Some(value), optPayeeSumWithChange, nPayed + 1)
                }

        }

        val inTxId = TxId(ByteString.fromString("inTxId"))
        val lockTxId = TxId(ByteString.fromString("lockTxId"))
        val currentTxId = TxId(ByteString.fromString("currentTxId"))

        val txCert = TxCert.RegStaking(Credential.PubKeyCredential(pkhA), Option.None)
        val txOutRef = TxOutRef(lockTxId, 0)

        val context = ScriptContext(
          txInfo = TxInfo(
            inputs = scalus.prelude.List(
              TxInInfo(
                outRef = TxOutRef(inTxId, 0),
                resolved = TxOut(
                  address = Address(PubKeyCredential(pkhA), Option.None),
                  value = Value.lovelace(10)
                )
              ),
              TxInInfo(
                outRef = TxOutRef(lockTxId, 0),
                resolved = TxOut(
                  address = Address(ScriptCredential(scriptHash), Option.None),
                  value = Value.lovelace(30)
                )
              )
            ),
            outputs = List(
              TxOut(
                address = Address(
                  PubKeyCredential(pkhA),
                  Option.None
                ),
                value = Value.lovelace(38)
              )
            ),
            fee = 2,
            certificates = scalus.prelude.List(txCert),
            signatories = scalus.prelude.List(pkhA),
            redeemers = AssocMap.fromList(
              scalus.prelude.List((ScriptPurpose.Spending(txOutRef), Data.unit))
            ),
            id = currentTxId
          ),
          redeemer = Data.unit,
          scriptInfo = ScriptInfo.SpendingScript(txOutRef = txOutRef),
        )

        val paramsData = List(pkhA).toData

        val lw = sir.toLoweredValue(generateErrorTraces = true)
        println(lw.pretty.render(100))

        val uplc = sir.toUplc(generateErrorTraces = true)
        println(uplc.pretty.render(100))

        val input = context.txInfo.inputs.head

        val applied = uplc.plutusV3

        val result = applied.evaluateDebug

        assert(result.isSuccess, s"Result: $result")

    }

}
