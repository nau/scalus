package scalus

import com.bloxbean.cardano.client.account.Account
import com.bloxbean.cardano.client.address.{Address, AddressService}
import com.bloxbean.cardano.client.api.UtxoSupplier
import com.bloxbean.cardano.client.api.exception.ApiRuntimeException
import com.bloxbean.cardano.client.api.model.{Amount, Result, Utxo}
import com.bloxbean.cardano.client.backend.api.{DefaultProtocolParamsSupplier, DefaultUtxoSupplier, TransactionService}
import com.bloxbean.cardano.client.backend.blockfrost.common.Constants
import com.bloxbean.cardano.client.backend.blockfrost.excpetion.BlockfrostConfigurationException
import com.bloxbean.cardano.client.backend.blockfrost.service.BFBackendService
import com.bloxbean.cardano.client.coinselection.impl.LargestFirstUtxoSelectionStrategy
import com.bloxbean.cardano.client.common.ADAConversionUtil.adaToLovelace
import com.bloxbean.cardano.client.common.CardanoConstants
import com.bloxbean.cardano.client.common.CardanoConstants.LOVELACE
import com.bloxbean.cardano.client.common.model.Networks
import com.bloxbean.cardano.client.crypto.Blake2bUtil
import com.bloxbean.cardano.client.function.helper.SignerProviders.signerFrom
import com.bloxbean.cardano.client.function.helper.model.ScriptCallContext
import com.bloxbean.cardano.client.function.helper.{BalanceTxBuilders, CollateralBuilders, InputBuilders, ScriptCallContextProviders}
import com.bloxbean.cardano.client.function.{Output, TxBuilder, TxBuilderContext, TxSigner}
import com.bloxbean.cardano.client.transaction.model.{PaymentTransaction, TransactionDetailsParams}
import com.bloxbean.cardano.client.transaction.spec.PlutusV1Script.PlutusV1ScriptBuilder
import com.bloxbean.cardano.client.transaction.spec.*
import com.bloxbean.cardano.client.function.helper.ScriptUtxoFinders
import com.bloxbean.cardano.client.transaction.spec.script.ScriptPubkey
import com.bloxbean.cardano.client.util.HexUtil
import com.google.common.collect.Lists
import io.bullet.borer.Cbor
import scalus.builtins.ByteString
import scalus.uplc.Data
import scalus.uplc.ToDataInstances.given
import scalus.utils.Utils

import java.math.BigInteger
import java.util.Collections

object SendTx:

  def dataToCardanoClientPlutusData(data: Data): PlutusData =
    import scala.jdk.CollectionConverters.*
    data match
      case Data.Constr(tag, args) =>
        val convertedArgs = ListPlutusData
          .builder()
          .plutusDataList(args.map(dataToCardanoClientPlutusData).asJava)
          .build()
        ConstrPlutusData
          .builder()
          .alternative(tag)
          .data(convertedArgs)
          .build()
      case Data.Map(items) =>
        MapPlutusData
          .builder()
          .map(
            items
              .map { case (k, v) =>
                (dataToCardanoClientPlutusData(k), dataToCardanoClientPlutusData(v))
              }
              .toMap
              .asJava
          )
          .build()
      case Data.List(items) =>
        ListPlutusData
          .builder()
          .plutusDataList(items.map(dataToCardanoClientPlutusData).asJava)
          .build()
      case Data.I(i) =>
        BigIntPlutusData.of(i.bigInteger)
      case Data.B(b) =>
        BytesPlutusData.of(b.bytes)

  def readFromFile(file: String): String =
    val source = scala.io.Source.fromFile(file)
    val lines =
      try source.mkString
      finally source.close()
    lines

  def readMnemonic(): String = readFromFile("mnemonic.txt")
  def readBlockfrostApiKey(): String = readFromFile("blockfrost_api_key.txt")

  val sender = new Account(Networks.testnet(), readMnemonic())

  val backendService =
    new BFBackendService(Constants.BLOCKFROST_TESTNET_URL, readBlockfrostApiKey())

  val utxoSupplier = new DefaultUtxoSupplier(backendService.getUtxoService)
  val protocolParamsSupplier = new DefaultProtocolParamsSupplier(backendService.getEpochService)

  def publishLockingTx(scriptAddressBech32: String, datum: Data) = {
    val senderAddress = sender.getBaseAddress.toBech32
    val lockOutput = Output
      .builder()
      .address(scriptAddressBech32)
      .assetName(LOVELACE)
      .qty(adaToLovelace(2))
      .datum(dataToCardanoClientPlutusData(datum))
      .build();

    val lockFundTxBuilder = lockOutput
      .outputBuilder()
      .buildInputs(InputBuilders.createFromSender(senderAddress, senderAddress))
      .andThen(BalanceTxBuilders.balanceTx(senderAddress, 1))

    val signedTx = TxBuilderContext
      .init(utxoSupplier, protocolParamsSupplier)
      .buildAndSign(lockFundTxBuilder, signerFrom(sender));

    println(signedTx)

    backendService.getTransactionService.submitTransaction(signedTx.serialize)
  }

  def spendLockedTx(
      script: PlutusScript,
      scriptAddressBech32: String,
      datum: Data,
      redeemer: Data,
      pubKeyHashBytes: Array[Byte]
  ) = {
    val senderAddress = sender.getBaseAddress.toBech32
    val scriptUtxo = ScriptUtxoFinders
      .findFirstByDatumHashUsingDatum(
        utxoSupplier,
        scriptAddressBech32,
        dataToCardanoClientPlutusData(datum)
      )
      .orElseThrow
    val claimAmount = scriptUtxo.getAmount.stream
      .filter((amount) => LOVELACE.equals(amount.getUnit))
      .findFirst
      .orElseThrow
      .getQuantity

    val output = Output.builder.address(senderAddress).assetName(LOVELACE).qty(claimAmount).build

    val scriptCallContext = ScriptCallContext.builder
      .script(script)
      .datum(dataToCardanoClientPlutusData(datum))
      .exUnits(
        ExUnits.builder
          .mem // Exact exUnits will be calculated later
          (BigInteger.valueOf(0))
          .steps(BigInteger.valueOf(0))
          .build
      )
      .redeemer(dataToCardanoClientPlutusData(redeemer))
      .redeemerTag(RedeemerTag.Spend)
      .build

    val signer = signerFrom(sender)

    val utxoSelectionStrategy = new LargestFirstUtxoSelectionStrategy(utxoSupplier)
    val collateralUtxos =
      utxoSelectionStrategy.select(
        senderAddress,
        new Amount(LOVELACE, adaToLovelace(5)),
        Collections.emptySet()
      )

    val contractTxBuilder = output.outputBuilder
      .buildInputs(InputBuilders.createFromUtxos(java.util.List.of(scriptUtxo)))
      .andThen(
        CollateralBuilders.collateralOutputs(senderAddress, Lists.newArrayList(collateralUtxos))
      )
      .andThen(ScriptCallContextProviders.createFromScriptCallContext(scriptCallContext))
      .andThen((context, txn) => {
        txn.getBody.getRequiredSigners.add(pubKeyHashBytes)
        val signedTx = signer.sign(txn)
        println(s"res: ${signedTx}")
        val res = backendService.getTransactionService.evaluateTx(signedTx.serialize())
        println(s"res: ${res}")
        val exUnits = res.getValue.get(0).getExUnits
        println(s"ExUnits: $exUnits")
        txn.getWitnessSet().getRedeemers().get(0).setExUnits(exUnits)
      })
      .andThen(BalanceTxBuilders.balanceTx(senderAddress, 1))

    val txBuilderContext = TxBuilderContext.init(utxoSupplier, protocolParamsSupplier)

    // Tx Build and Submit
    val txn = txBuilderContext.build(contractTxBuilder)
    val signedTx = signer.sign(txn)
    backendService.getTransactionService.submitTransaction(signedTx.serialize)
  }

  def main(args: Array[String]): Unit =
    val cborHex = OptimizedPreimage.doubleCborHex
    val script = PlutusV1Script.builder().cborHex(cborHex).build()
    val scriptAddress = AddressService.getInstance().getEntAddress(script, Networks.testnet())
    val scriptAddressBech32 = scriptAddress.toBech32()
    val preimage = "Scalus rocks!"
    val preimageBytes = preimage.getBytes("UTF-8")
    val preimageHash = Utils.bytesToHex(Utils.sha2_256(preimageBytes))
    val pubKeyHashBytes = sender.hdKeyPair().getPublicKey.getKeyHash()
    val pubKeyHash = Utils.bytesToHex(pubKeyHashBytes)
    import scalus.uplc.Data.toData
    implicit val enc = scalus.uplc.PlutusDataCborEncoder
    val datum = (ByteString.fromHex(preimageHash), ByteString.fromHex(pubKeyHash)).toData
    val datumCbor = Cbor.encode(datum).toByteArray
    val datumHash = Blake2bUtil.blake2bHash256(datumCbor)
    val datumHashHex = Utils.bytesToHex(datumHash)
    val redeemer = ByteString.fromArray(preimageBytes).toData
    println(s"Script SIR")
    println(OptimizedPreimage.compiledOptimizedPreimageValidator.pretty.render(100))
    println(s"Script double CBOR: ${OptimizedPreimage.doubleCborHex}")
    println(s"Script Testnet Address: ${scriptAddressBech32}")
    println(s"Script Hash: ${Utils.bytesToHex(script.getScriptHash())}")
    println(s"Preimage: $preimage, Hex: ${Utils.bytesToHex(preimageBytes)}, Hash: $preimageHash")
    println(s"PubKeyHash : $pubKeyHash")
    println(s"Datum: $datum, CBOR: ${Utils.bytesToHex(datumCbor)}")
    println(s"Datum Hash: $datumHashHex")

    def lock() = println(publishLockingTx(scriptAddressBech32 = scriptAddressBech32, datum = datum))
    def spend() = println(
      spendLockedTx(
        scriptAddressBech32 = scriptAddressBech32,
        script = script,
        datum = datum,
        redeemer = redeemer,
        pubKeyHashBytes = pubKeyHashBytes
      )
    )

    // lock()
    // spend()
