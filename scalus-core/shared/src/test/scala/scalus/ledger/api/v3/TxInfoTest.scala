package scalus.ledger.api.v3

import scalus.prelude.StdlibTestKit
import scalus.prelude.{List, Option, SortedMap, given}
import scalus.builtin.ByteString
import scalus.ledger.api.v2.OutputDatum

class TxInfoTest extends StdlibTestKit with ArbitraryInstances {
    test("findOwnInput") {
        checkEval { (txOutRef: TxOutRef) =>
            TxInfo.placeholder.findOwnInput(txOutRef).isEmpty
        }

        // TODO: UPLC error -- need support to copy in case class
//        checkEval { (txInfo: TxInfo, txInInfo: TxInInfo) =>
//            val newTxInfo = txInfo.copy(inputs = txInInfo +: txInfo.inputs)
//            newTxInfo.findOwnInput(txInInfo.outRef) === Option.Some(txInInfo)
//        }

        assertEval(
          TxInfo.placeholder
              .findOwnInput(
                TxOutRef(
                  TxInfo.placeholder.id,
                  BigInt(0)
                )
              )
              .isEmpty
        )

        // TODO: UPLC error
//        assertEvalEq(
//          TxInfo.placeholder
//              .copy(
//                inputs = List(
//                  TxInInfo(
//                    TxOutRef(TxInfo.placeholder.id, BigInt(0)),
//                    TxOut(
//                      Address(
//                        Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
//                        Option.None
//                      ),
//                      Value.zero
//                    )
//                  )
//                )
//              )
//              .findOwnInput(
//                TxOutRef(TxInfo.placeholder.id, BigInt(0))
//              ),
//          Option.Some(
//            TxInInfo(
//              TxOutRef(TxInfo.placeholder.id, BigInt(0)),
//              TxOut(
//                Address(
//                  Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
//                  Option.None
//                ),
//                Value.zero
//              )
//            )
//          )
//        )
    }

    test("findOwnDatum") {
        // TODO: UPLC error
//        checkEval { (datumHash: DatumHash) =>
//            TxInfo.placeholder.findOwnDatum(datumHash).isEmpty
//        }

        // TODO: UPLC error
//        checkEval { (txInfo: TxInfo, datum: Datum) =>
//            val newTxInfo = txInfo.copy(
//              data = SortedMap.singleton(datum.dataHash, datum),
//              outputs = List.empty
//            )
//
//            newTxInfo.findOwnDatum(datum.dataHash) === Option.Some(datum)
//        }

        // TODO: UPLC error
//        checkEval { (txInfo: TxInfo, datum: Datum) =>
//            val newTxInfo = txInfo.copy(
//              data = SortedMap.empty,
//              outputs = List(
//                TxOut(
//                  Address(
//                    Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
//                    Option.None
//                  ),
//                  Value.zero,
//                  OutputDatum.OutputDatum(datum)
//                )
//              )
//            )
//
//            newTxInfo.findOwnDatum(datum.dataHash) === Option.Some(datum)
//        }

        // TODO: UPLC error
//        assertEval(
//          TxInfo.placeholder.findOwnDatum(Data.unit.dataHash).isEmpty
//        )

        // TODO: UPLC error
//        assertEvalEq(
//          TxInfo.placeholder
//              .copy(
//                data = SortedMap.singleton(Data.unit.dataHash, Data.unit),
//                outputs = List.empty
//              )
//              .findOwnDatum(Data.unit.dataHash),
//          Option.Some(Data.unit)
//        )

        // TODO: UPLC error
//        assertEvalEq(
//          TxInfo.placeholder
//              .copy(
//                data = SortedMap.empty,
//                outputs = List(
//                  TxOut(
//                    Address(
//                      Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
//                      Option.None
//                    ),
//                    Value.zero,
//                    OutputDatum.OutputDatum(Data.unit)
//                  )
//                )
//              )
//              .findOwnDatum(Data.unit.dataHash),
//          Option.Some(Data.unit)
//        )
    }

    test("findOwnScriptOutputs") {
        checkEval { (validatorHash: ValidatorHash) =>
            TxInfo.placeholder.findOwnScriptOutputs(validatorHash).isEmpty
        }

        // TODO: UPLC error
//        checkEval { (txInfo: TxInfo, validatorHash: ValidatorHash) =>
//            val newTxInfo = txInfo.copy(
//              outputs = List(
//                TxOut(
//                  Address(
//                    Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
//                    Option.None
//                  ),
//                  Value.zero
//                ),
//                TxOut(
//                  Address(
//                    Credential.ScriptCredential(validatorHash),
//                    Option.None
//                  ),
//                  Value.zero
//                )
//              )
//            )
//
//            newTxInfo.findOwnScriptOutputs(validatorHash) === List(
//              TxOut(
//                Address(
//                  Credential.ScriptCredential(validatorHash),
//                  Option.None
//                ),
//                Value.zero
//              )
//            )
//        }

        assertEval(
          TxInfo.placeholder
              .findOwnScriptOutputs(ByteString.empty)
              .isEmpty
        )

        // TODO: UPLC error
//        assertEvalEq(
//          TxInfo.placeholder
//              .copy(
//                outputs = List(
//                  TxOut(
//                    Address(
//                      Credential.PubKeyCredential(PubKeyHash(ByteString.empty)),
//                      Option.None
//                    ),
//                    Value.zero
//                  ),
//                  TxOut(
//                    Address(
//                      Credential.ScriptCredential(ByteString.empty),
//                      Option.None
//                    ),
//                    Value.zero
//                  )
//                )
//              )
//              .findOwnScriptOutputs(ByteString.empty),
//          List(
//            TxOut(
//              Address(
//                Credential.ScriptCredential(ByteString.empty),
//                Option.None
//              ),
//              Value.zero
//            )
//          )
//        )
    }
}
