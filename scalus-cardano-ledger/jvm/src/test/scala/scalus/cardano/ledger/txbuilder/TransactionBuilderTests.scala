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
import scalus.cardano.ledger.txbuilder.TxBuilderLenses.txBodyL
import io.bullet.borer.Cbor
import monocle.syntax.all.*
import monocle.{Focus, Lens}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import scalus.cardano.ledger.ArbitraryInstances.given
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
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import TransactionBuilder.txBodyL

import scala.collection.immutable.{SortedMap, SortedSet}

private def addInput(input: TransactionInput): Transaction => Transaction =
    txBodyL
        .refocus(_.inputs)
        .modify((is: TaggedOrderedSet[TransactionInput]) =>
            TaggedOrderedSet.from(
              is.toSortedSet + input
            )
        )

class TxBuilderTests extends AnyFunSuite, ScalaCheckPropertyChecks {

    /** Test that the builder steps fail with the expected error
      *
      * @param label
      * @param steps
      * @param error
      */
    def testBuilderStepsFail(
        label: String,
        steps: Seq[TransactionBuilderStep],
        error: TxBuildError
    ): Unit =
        test(label) {
            val res = TransactionBuilder.build(Mainnet, steps)
            assert(res == Left(error))
        }

    def testBuilderSteps(
        label: String,
        steps: Seq[TransactionBuilderStep],
        expected: (
            Transaction,
            Seq[DetachedRedeemer],
            Network,
            Set[ExpectedSigner],
            Set[TransactionUnspentOutput]
        )
    ): Unit =
        test(label) {
            val res = TransactionBuilder.build(Mainnet, steps)
            assert(res.map(_.toTuple) == Right(expected))
        }

    val pkhUtxo = TransactionUnspentOutput(input = input1, output = pkhOutput)
    val skhUtxo = TransactionUnspentOutput(input1, skhOutput)
    val ns: Script.Native = Script.Native(AllOf(IndexedSeq.empty))
    val nsSigners: Set[AddrKeyHash] = Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet
    val nsWitness = NativeScriptOutput(ScriptValue(ns, nsSigners))

    val psSignersOutput: Set[AddrKeyHash] = Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet

    val plutusScriptWitness =
        PlutusScriptOutput(ScriptValue(script2, psSignersOutput), Data.List(List()), None)

    val psSignersRef: Set[AddrKeyHash] = Gen.listOf(arbitrary[AddrKeyHash]).sample.get.toSet
    val plutusScriptRefWitness = PlutusScriptOutput(
      ScriptWitness.ScriptReference(input1, SpendInput, psSignersRef),
      Data.List(List()),
      None
    )

    ///////////////////////////////////////////////////////////////
    // Group: "SpendOutput"
    ///////////////////////////////////////////////////////////////

    val spendPkhUtxoStep = TransactionBuilderStep.SpendOutput(pkhUtxo, None)
    val pubKeyInput1Expected: ContextTuple =
        Context.empty(Mainnet).toTuple
            |> transactionL
                .andThen(txBodyL.refocus(_.inputs))
                .replace(TaggedOrderedSet(input1))
            |> expectedSignersL
                .modify(_ ++ fromRight(spendPkhUtxoStep.additionalSigners))

    testBuilderSteps(
      label = "PKH Output",
      steps = List(SpendOutput(pkhUtxo, None)),
      expected = pubKeyInput1Expected
    )

    testBuilderSteps(
      label = "PKH output x2 -> 1",
      steps = List(SpendOutput(pkhUtxo, None), SpendOutput(pkhUtxo, None)),
      expected = pubKeyInput1Expected
    )

    testBuilderStepsFail(
      label = "PKH output with wrong witness #1",
      steps = List(SpendOutput(pkhUtxo, Some(nsWitness))),
      error = WrongOutputType(ScriptHashWitness(nsWitness), pkhUtxo)
    )

    testBuilderStepsFail(
      label = "PKH output with wrong witness #2",
      steps = List(SpendOutput(pkhUtxo, Some(plutusScriptRefWitness))),
      error = WrongOutputType(ScriptHashWitness(plutusScriptRefWitness), pkhUtxo)
    )

    testBuilderStepsFail(
      label = "SKH output with wrong witness #1",
      steps = List(SpendOutput(skhUtxo, Some(nsWitness))),
      error = IncorrectScriptHash(Left(ns), scriptHash1)
    )

    testBuilderStepsFail(
      label = "SKH output with wrong witness #2",
      steps = List(SpendOutput(skhUtxo, Some(plutusScriptWitness))),
      error = IncorrectScriptHash(Right(script2), scriptHash1)
    )

    test("PKH output with wrong NetworkId") {
        val pkhUtxoTestNet =
            TransactionUnspentOutput(
              input = input0,
              output = Babbage(
                address = ShelleyAddress(
                  network = Testnet,
                  payment = pkhOutputPaymentPart,
                  delegation = Null
                ),
                value = Value.zero,
                datumOption = None,
                scriptRef = None
              )
            )
        val res = modify(
          Context.empty(Mainnet),
          List(SpendOutput(pkhUtxo, None), SpendOutput(pkhUtxoTestNet, None))
        ) // Mainnet context with mixed network addresses
        assert(res == Left(WrongNetworkId(pkhUtxoTestNet.output.address)))
    }

    // ================================================================
    // Subgroup: Signature tracking
    // ================================================================

    test("SpendOutput.additionalSignersUnsafe works for pubkey") {

        // Check that the additional signers are what we expect
        assert(
          spendPkhUtxoStep.additionalSigners == Right(
            Set(
              ExpectedSigner(
                pkhOutputPaymentPart.asInstanceOf[ShelleyPaymentPart.Key].hash
              )
            )
          )
        )

        // Check that the transaction step adds the correct signer
        val tx = build(Mainnet, List(spendPkhUtxoStep))
        assert(tx.map(_.expectedSigners) == Right(fromRight(spendPkhUtxoStep.additionalSigners)))

    }

    test("Signers works for NS spend") {
        val txInput = arbitrary[TransactionInput].sample.get

        val step =
            TransactionBuilderStep.SpendOutput(
              utxo = TransactionUnspentOutput(
                txInput,
                Babbage(
                  address = ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(ns.scriptHash), Null),
                  value = Value.zero,
                  datumOption = None,
                  scriptRef = None
                )
              ),
              witness = Some(nsWitness)
            )

        // Signers are what we expected for an NS spend
        assert(step.additionalSigners == Right(nsSigners.map(ExpectedSigner(_))))

        // Signers are what we expect for a transaction built with this step
        assert(
          build(Mainnet, List(step)).map(_.expectedSigners) == Right(
            fromRight(step.additionalSigners)
          )
        )
    }

    test("Signers work for PS spend") {
        val txInput = arbitrary[TransactionInput].sample.get
        val step =
            TransactionBuilderStep.SpendOutput(
              utxo = TransactionUnspentOutput(
                txInput,
                Babbage(
                  address =
                      ShelleyAddress(Mainnet, ShelleyPaymentPart.Script(script2.scriptHash), Null),
                  value = Value.zero,
                  datumOption = Some(Inline(Data.List(List.empty))),
                  scriptRef = None
                )
              ),
              witness = Some(plutusScriptWitness)
            )

        // Signers are what we expected for an PS spend
        assert(fromRight(step.additionalSigners) == psSignersOutput.map(ExpectedSigner(_)))

        // Signers are what we expect for a transaction built with this step
        assert(
          build(Mainnet, List(step)).map(_.expectedSigners) == Right(
            fromRight(step.additionalSigners)
          )
        )
    }

    // =======================================================================
    // Group: "Pay"
    // =======================================================================

    testBuilderSteps(
      label = "Pay #1",
      steps = List(Pay(pkhOutput)),
      expected = Context.empty(Mainnet).toTuple
          |> transactionL
              .andThen(txBodyL.refocus(_.outputs))
              .replace(IndexedSeq(Sized(pkhOutput)))
    )

    // =======================================================================
    // Group: "MintAsset"
    // =======================================================================

    // NOTE (dragospe, 2025-09-24): upstream, this test is the same as pay #1. I've modified it.
    testBuilderSteps(
      label = "MintAsset #1",
      steps = List(
        MintAsset(
          scriptHash = scriptHash1,
          assetName = AssetName(ByteString.fromHex("deadbeef")),
          amount = 1L,
          witness = CredentialWitness.PlutusScriptCredential(
            ScriptWitness.ScriptValue(script1, Set.empty),
            redeemer = Data.List(List.empty)
          )
        )
      ),
      expected = Context.empty(Mainnet).toTuple |>
          // replace mint
          transactionL
              .andThen(txBodyL)
              .refocus(_.mint)
              .replace(
                Some(
                  Mint(
                    MultiAsset(
                      SortedMap.from(
                        List(
                          scriptHash1 -> SortedMap.from(
                            List(AssetName(ByteString.fromHex("deadbeef")) -> 1L)
                          )
                        )
                      )
                    )
                  )
                )
              )
          |>
          // add script witness
          transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .replace(Set(script1))
          |>
          // add redeemer
          transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(
                Some(
                  KeepRaw(
                    Redeemers(
                      Redeemer(
                        tag = RedeemerTag.Mint,
                        index = 0,
                        data = Data.List(List.empty),
                        exUnits = ExUnits.zero
                      )
                    )
                  )
                )
              )
          |>
          redeemersL.replace(
            List(
              DetachedRedeemer(
                datum = Data.List(List.empty),
                purpose = ForMint(
                  ScriptHash.fromHex("36137e3d612d23a644283f10585958085aa255bdae4076fcefe414b6")
                )
              )
            )
          )
    )

    // =======================================================================
    // Group: "Deregister"
    // =======================================================================

    testBuilderSteps(
      label = "Deregister script",
      steps = List(
        IssueCertificate(
          cert = Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None),
          witness = Some(
            PlutusScriptCredential(
              ScriptWitness.ScriptValue(script1, Set.empty),
              Data.List(List.empty)
            )
          )
        )
      ),
      expected = Context.empty(Mainnet).toTuple |>
          transactionL
              .refocus(_.witnessSet.plutusV1Scripts)
              .replace(Set(script1)) |>
          transactionL
              .refocus(_.witnessSet.redeemers)
              .replace(
                Some(
                  KeepRaw(
                    Redeemers(
                      Redeemer(
                        tag = Cert,
                        index = 0,
                        data = Data.List(List.empty),
                        exUnits = ExUnits.zero
                      )
                    )
                  )
                )
              )
          |>
          transactionL
              .andThen(txBodyL)
              .refocus(_.certificates)
              .replace(
                TaggedSet(
                  Certificate.UnregCert(Credential.ScriptHash(script1.scriptHash), coin = None)
                )
              )
          |>
          redeemersL.replace(
            List(
              DetachedRedeemer(
                datum = Data.List(List.empty),
                purpose = ForCert(
                  UnregCert(
                    Credential.ScriptHash(
                      ScriptHash.fromHex("36137e3d612d23a644283f10585958085aa255bdae4076fcefe414b6")
                    ),
                    None
                  )
                )
              )
            )
          )
    )

    val witness =
        PlutusScriptCredential(ScriptWitness.ScriptValue(script1, Set.empty), Data.List(List.empty))

    testBuilderStepsFail(
      label = "Deregistering stake credential with unneeded witness fails",
      steps = List(IssueCertificate(UnregCert(pubKeyHashCredential1, coin = None), Some(witness))),
      error = UnneededDeregisterWitness(StakeCredential(pubKeyHashCredential1), witness)
    )

    testBuilderStepsFail(
      label = "deregistering stake credential with wrong witness fails",
      steps = List(
        IssueCertificate(
          cert = UnregCert(Credential.ScriptHash(script2.scriptHash), coin = None),
          witness =
              Some(PlutusScriptCredential(ScriptValue(script1, Set.empty), Data.List(List.empty)))
        )
      ),
      error = IncorrectScriptHash(Right(script1), script2.scriptHash)
    )

    // =======================================================================
    // Group: "Modify Aux Data"
    // =======================================================================
    testBuilderSteps(
      label = "ModifyAuxData: id",
      steps = List(ModifyAuxData(identity)),
      expected = Context.empty(Mainnet).toTuple
    )

}

// ===========================================================================
// Test Helpers
// ===========================================================================

// ===========================================================================
// Test Data
// ===========================================================================

val unitRedeemer: Redeemer =
    Redeemer(
      tag = Spend,
      index = 0,
      data = ByteString.fromHex("").toData,
      exUnits = ExUnits.zero
    )

val script1: Script.PlutusV1 = {
    val bytes = ByteString.fromHex("4d01000033222220051200120011").bytes
    Cbor.decode(bytes).to[Script.PlutusV1].value
}

val scriptHash1: ScriptHash = script1.scriptHash

val scriptHashCredential1: Credential = Credential.ScriptHash(scriptHash1)

val skhOutput: TransactionOutput.Babbage = Babbage(
  address = ShelleyAddress(
    network = Mainnet,
    payment = ShelleyPaymentPart.Script(scriptHashCredential1.scriptHashOption.get),
    delegation = null
  ),
  value = Value(Coin(5_000_000L)),
  datumOption = None,
  scriptRef = None
)

val pubKeyHashCredential1: Credential = {
    val bytes: Array[Byte] = Array(57, 3, 16, 58, 231, 6, 129, 67, 155, 84, 118, 254, 245, 159, 67,
      155, 139, 200, 109, 132, 191, 178, 211, 118, 252, 63, 86, 23).map(_.toByte)
    Credential.KeyHash(Hash(ByteString.fromArray(bytes)))
}

val pkhOutputPaymentPart: ShelleyPaymentPart = {
    val bytes: Array[Byte] = Array(243, 63, 250, 132, 253, 242, 10, 0, 52, 67, 165, 226, 118, 142,
      18, 233, 45, 179, 21, 53, 220, 166, 32, 136, 177, 83, 223, 36).map(_.toByte)
    ShelleyPaymentPart.Key(Hash(ByteString.fromArray(bytes)))
}

val pkhOutput: Babbage = Babbage(
  address = ShelleyAddress(
    network = Mainnet,
    payment = pkhOutputPaymentPart,
    delegation = Key(pubKeyHashCredential1.keyHashOption.get.asInstanceOf[StakeKeyHash])
  ),
  value = Value(Coin(5_000_000L)),
  datumOption = None,
  scriptRef = None
)

def mkTransactionInput(txId: String, ix: Int): TransactionInput = {
    val txIdBytes: Array[Byte] = ByteString.fromHex(txId).bytes
    TransactionInput(
      transactionId = Hash(ByteString.fromArray(txIdBytes)),
      index = ix
    )
}

val input0: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 0)

val input1: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 1)

val input2: TransactionInput =
    mkTransactionInput("5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996", 2)

val script2: Script.PlutusV1 =
    Cbor.decode(ByteString.fromHex("4e4d01000033222220051200120012").bytes)
        .to[Script.PlutusV1]
        .value

val anyNetworkTx: Transaction = Transaction.empty

// See: https://github.com/mlabs-haskell/purescript-cardano-types/blob/348fbbefa8bec5050e8492f5a9201ac5bb17c9d9/test/CSLHex.purs#L109
val testnetTransaction: Transaction =
    txBodyL.refocus(_.networkId).replace(Some(0))(anyNetworkTx)

val testnetContext: ContextTuple =
    Context.empty(Testnet).toTuple |> transactionL.replace(testnetTransaction)

private def fromRight[A, B](e: Either[A, B]): B =
    e match { case Right(x) => x }

// The fields of a Context, to cut down on noise
private type ContextTuple = (
    Transaction,
    Seq[DetachedRedeemer],
    Network,
    Set[ExpectedSigner],
    Set[TransactionUnspentOutput]
)

def transactionL: Lens[ContextTuple, Transaction] = Focus[ContextTuple](_._1)
def redeemersL: Lens[ContextTuple, Seq[DetachedRedeemer]] = Focus[ContextTuple](_._2)
def networkL: Lens[ContextTuple, Network] = Focus[ContextTuple](_._3)
def expectedSignersL: Lens[ContextTuple, Set[ExpectedSigner]] = Focus[ContextTuple](_._4)
def resolvedUtxosL: Lens[ContextTuple, Set[TransactionUnspentOutput]] = Focus[ContextTuple](_._5)
