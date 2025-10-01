package scalus.cardano.ledger.txbuilder

import scalus.cardano.ledger.txbuilder.*
import scalus.cardano.ledger.txbuilder.CredentialWitness.PlutusScriptCredential
import scalus.cardano.ledger.txbuilder.ExpectedWitnessType.ScriptHashWitness
import scalus.cardano.ledger.txbuilder.InputAction.SpendInput
import scalus.cardano.ledger.txbuilder.OutputWitness.{NativeScriptOutput, PlutusScriptOutput}
import scalus.cardano.ledger.txbuilder.RedeemerPurpose.{ForCert, ForMint}
import scalus.cardano.ledger.txbuilder.ScriptWitness.ScriptValue
import scalus.cardano.ledger.txbuilder.TransactionBuilder.{build, modify, Context}
import scalus.cardano.ledger.txbuilder.TransactionBuilderStep.*
import scalus.cardano.ledger.txbuilder.TransactionEditor.{editTransaction, editTransactionSafe}
import scalus.cardano.ledger.txbuilder.TxBuildError.{IncorrectScriptHash, UnneededDeregisterWitness, WrongNetworkId, WrongOutputType}
import hydrozoa.{emptyTransaction, txBodyL}
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import org.scalacheck.Gen
import scalus.builtin.Data.toData
import scalus.builtin.{ByteString, Data}
import scalus.cardano.address.Network.{Mainnet, Testnet}
import scalus.cardano.address.ShelleyDelegationPart.{Key, Null}
import scalus.cardano.address.{Network, ShelleyAddress, ShelleyPaymentPart}
import scalus.cardano.ledger.*
import scalus.cardano.ledger.Certificate.UnregCert
import scalus.cardano.ledger.DatumOption.Inline
import scalus.cardano.ledger.RedeemerTag.{Cert, Spend}
import scalus.cardano.ledger.Timelock.AllOf
import scalus.cardano.ledger.TransactionOutput.Babbage
import scalus.|>
import test.{genAddrKeyHash, genTxId}

import scala.collection.immutable.{SortedMap, SortedSet}

private def addInput(input: TransactionInput): Transaction => Transaction =
    txBodyL
        .refocus(_.inputs)
        .modify((is: TaggedOrderedSet[TransactionInput]) =>
            TaggedOrderedSet.from(
              is.toSortedSet + input
            )
        )

class TransactionEditorTests extends munit.ScalaCheckSuite {

    val oneInput: Transaction = {
        val l1 = txBodyL
            .refocus(_.inputs)
            .replace(TaggedOrderedSet(input1))
        val l2 = Focus[Transaction](_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = ByteString.fromHex("").toData,
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )
        anyNetworkTx |> l1 |> l2
    }

    test("do nothing") {
        assertEquals(obtained = editTransaction(identity)(oneInput), expected = oneInput)
    }

    test("attach one input to the end") {
        val tx1 = txBodyL.refocus(_.inputs).replace(TaggedOrderedSet(input1, input2))(anyNetworkTx)
        val expectedTx = tx1 |> Focus[Transaction](_.witnessSet.redeemers)
            .replace(
              Some(
                KeepRaw(
                  Redeemers(
                    Redeemer(
                      tag = Spend,
                      index = 0,
                      data = ByteString.fromHex("").toData,
                      exUnits = ExUnits.zero
                    )
                  )
                )
              )
            )

        assertEquals(
          obtained = editTransaction(
            txBodyL
                .refocus(_.inputs)
                .modify((i: TaggedOrderedSet[TransactionInput]) =>
                    TaggedOrderedSet.from(i.toSeq :+ input2)
                )
          )(
            oneInput
          ),
          expected = expectedTx
        )
    }

    test("remove two inputs, before and after") {
        val tx1 = {
            val l1 =
                Focus[Transaction](_.witnessSet.redeemers)
                    .replace(Some(KeepRaw(Redeemers(unitRedeemer.focus(_.index).replace(1)))))
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedOrderedSet(input0, input1, input2))
            anyNetworkTx |> l1 |> l2
        }
        val tx2 = {
            val l1 = Focus[Transaction](_.witnessSet.redeemers)
                .replace(Some(KeepRaw(Redeemers(unitRedeemer))))
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedOrderedSet(input1))
            anyNetworkTx |> l1 |> l2
        }

        assertEquals(
          obtained = editTransactionSafe(
            txBodyL.refocus(_.inputs).replace(TaggedOrderedSet(input1))
          )(tx1),
          expected = Right(tx2)
        )
    }

    test("remove two inputs with redeemers, before and after") {
        val tx1 = {
            val l1 = Focus[Transaction](_.witnessSet.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        unitRedeemer,
                        Redeemer(
                          tag = Spend,
                          index = 1,
                          data = Data.List(List()),
                          exUnits = ExUnits.zero
                        ),
                        Redeemer(
                          tag = Spend,
                          index = 2,
                          data = Data.Map(List.empty),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedOrderedSet(input0, input1, input2))
            anyNetworkTx |> l1 |> l2
        }
        val tx2 = {
            val l1 = Focus[Transaction](_.witnessSet.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        Redeemer(
                          tag = Spend,
                          index = 0,
                          data = Data.List(List.empty),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 =
                txBodyL
                    .refocus(_.inputs)
                    .replace(TaggedOrderedSet(input1))

            anyNetworkTx |> l1 |> l2
        }
        assertEquals(
          expected = Right(tx2),
          obtained = editTransactionSafe(
            txBodyL.refocus(_.inputs).replace(TaggedOrderedSet(input1))
          )(tx1)
        )
    }

    test("remove input & redeemer, add another input & redeemer") {
        val tx1 = {
            val l1 = Focus[Transaction](_.witnessSet.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        unitRedeemer,
                        Redeemer(
                          tag = Spend,
                          index = 1,
                          data = Data.Map(List.empty),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 = txBodyL
                .refocus(_.inputs)
                .replace(TaggedOrderedSet(input1, input2))
            anyNetworkTx |> l1 |> l2
        }

        val tx2 = {
            val l1 = Focus[Transaction](_.witnessSet.redeemers)
                .replace(
                  Some(
                    KeepRaw(
                      Redeemers(
                        Redeemer(
                          tag = Spend,
                          index = 1,
                          data = Data.Map(List.empty),
                          exUnits = ExUnits.zero
                        ),
                        Redeemer(
                          tag = Spend,
                          index = 0,
                          data = Data.List(List.empty),
                          exUnits = ExUnits.zero
                        )
                      )
                    )
                  )
                )
            val l2 =
                txBodyL
                    .refocus(_.inputs)
                    .replace(TaggedOrderedSet(input0, input2))
            anyNetworkTx |> l1 |> l2
        }
        assertEquals(
          expected = Right(tx2),
          obtained = tx1 |> editTransactionSafe(
            txBodyL
                .refocus(_.inputs)
                .replace(TaggedOrderedSet(input0, input2))
                .compose(
                  Focus[Transaction](_.witnessSet.redeemers)
                      .replace(
                        Some(
                          KeepRaw(
                            Redeemers(
                              Redeemer(
                                tag = Spend,
                                index = 0,
                                data = Data.List(List.empty),
                                exUnits = ExUnits.zero
                              )
                            )
                          )
                        )
                      )
                )
          )
        )
    }
}
