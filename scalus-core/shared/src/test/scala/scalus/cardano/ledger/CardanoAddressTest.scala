package scalus.cardano.ledger

import org.scalatest.funsuite.AnyFunSuite
import scalus.builtin.ByteString

import scala.util.Success

/** Test suite for CardanoAddress implementation Tests cover address encoding/decoding for different
  * address types based on CIP-19 test vectors
  */
class CardanoAddressTest extends AnyFunSuite {
    import CardanoAddress.*

    // Test vectors from CIP-19 and real Cardano addresses
    private val testVectors = List(
      // Format: (address_string, expected_type_id, description)
        // format: off
        ("addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x", 0, "Payment Key + Stake Key"),
        ("addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh", 1, "Script + Stake Key"),
        ("addr1yx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shs2z78ve", 2, "Payment Key + Script"),
        ("addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g", 3, "Script + Script"),
        ("addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k", 4, "Payment Key + Pointer"),
        ("addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu", 5, "Script + Pointer"),
        ("addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8", 6, "Payment Key Only"),
        ("addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx", 7, "Script Only"),
        ("stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw", 14, "Stake Key"),
        ("stake178phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcccycj5", 15, "Stake Script")
    )
    // format: on

    // Sample hash values for testing
    private val samplePaymentHash = AddrKeyHash(
      Hash28(ByteString.fromHex("c37b1b5dc0669f1d3c61a6fddb2e8fde96be87b881c60bce8e8d542f"))
    )
    private val sampleStakeHash = Hash28(
      ByteString.fromHex("337b62cfff6403a06a3acbc34f8c46003c69fe79a3628cefa9c47251")
    )
    private val sampleScriptHash = ScriptHash(
      Hash28(ByteString.fromHex("1234567890abcdef1234567890abcdef12345678901234567890abcd"))
    )

    test("Original test vectors should parse with bech32") {
        testVectors.foreach { case (addressStr, expectedTypeId, description) =>
            val parseResult = fromBech32(addressStr)
            assert(parseResult.typeId == expectedTypeId, s"Wrong type ID for $description")
        }
    }

    test("Network enum should handle all cases correctly") {
        assert(Network.fromByte(0x00) == Network.Testnet)
        assert(Network.fromByte(0x01) == Network.Mainnet)
        assert(Network.fromByte(0x05) == Network.Other(0x05))

        assert(Network.Testnet.value == 0x00)
        assert(Network.Mainnet.value == 0x01)
        assert(Network.Other(0x05).value == 0x05)

        assert(Network.Mainnet.isMainnet == true)
        assert(Network.Testnet.isMainnet == false)
    }

    test("Pointer should encode and decode correctly") {
        val pointer = Pointer(Slot(2498243), 27, 3)
        val encoded = pointer.toBytes

        // Test round-trip encoding/decoding
        val decoded = Pointer.fromBytes(encoded)
        assert(decoded == Success(pointer))
        // Test parsing from array with offset
        val Success(parsedPointer, bytesUsed) = Pointer.parseFrom(encoded, 0): @unchecked
        assert(parsedPointer == pointer)
        assert(bytesUsed == encoded.length)
    }

    test("Variable length encoding should work correctly") {
        // Test cases: (value, expected_bytes)
        val testCases = List(
          (0L, Array[Byte](0x00)),
          (127L, Array[Byte](0x7f)),
          (128L, Array[Byte](0x80.toByte, 0x01)),
          (16383L, Array[Byte](0xff.toByte, 0x7f)),
          (16384L, Array[Byte](0x80.toByte, 0x80.toByte, 0x01))
        )

        testCases.foreach { case (value, expectedBytes) =>
            val encoded = encodeVariableLengthUInt(value)
            assert(encoded.sameElements(expectedBytes), s"Encoding failed for value $value")

            val (decoded, bytesUsed) = decodeVariableLengthUInt(encoded, 0)
            assert(decoded == value)
            assert(bytesUsed == encoded.length)
        }
    }

    test("ShelleyPaymentPart should handle key and script hashes") {
        val keyPart = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val scriptPart = ShelleyPaymentPart.scriptHash(sampleScriptHash)

        assert(keyPart.asHash == samplePaymentHash.hash)
        assert(scriptPart.asHash == sampleScriptHash.hash)

        assert(keyPart.isScript == false)
        assert(scriptPart.isScript == true)

        assert(keyPart.toBytes == samplePaymentHash.hash.bytes)
        assert(scriptPart.toBytes == sampleScriptHash.hash.bytes)
    }

    test("ShelleyDelegationPart should handle all delegation types") {
        val keyDelegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val scriptDelegation = ShelleyDelegationPart.scriptHash(sampleScriptHash)
        val pointerDelegation = ShelleyDelegationPart.Pointer(Pointer(Slot(100), 5, 2))
        val nullDelegation = ShelleyDelegationPart.Null

        assert(keyDelegation.asHash == Some(sampleStakeHash))
        assert(scriptDelegation.asHash == Some(sampleScriptHash.hash))
        assert(pointerDelegation.asHash == None)
        assert(nullDelegation.asHash == None)

        assert(keyDelegation.isScript == false)
        assert(scriptDelegation.isScript == true)
        assert(pointerDelegation.isScript == false)
        assert(nullDelegation.isScript == false)
    }

    test("StakePayload should handle stake and script types") {
        val stakePayload = StakePayload.stakeKey(sampleStakeHash)
        val scriptPayload = StakePayload.script(sampleScriptHash)

        assert(stakePayload.asHash == sampleStakeHash)
        assert(scriptPayload.asHash == sampleScriptHash.hash)

        assert(stakePayload.isScript == false)
        assert(scriptPayload.isScript == true)

        // Test parsing from bytes
        val stakeFromBytes = StakePayload.fromBytes(sampleStakeHash.bytes.bytes, false)
        assert(stakeFromBytes == Success(StakePayload.Stake(sampleStakeHash)))

        val scriptFromBytes = StakePayload.fromBytes(sampleScriptHash.hash.bytes.bytes, true)
        assert(scriptFromBytes == Success(StakePayload.Script(sampleScriptHash)))
    }

    test("ShelleyAddress should calculate correct type IDs") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val addr = ShelleyAddress(Network.Mainnet, payment, delegation)
        assert(addr.typeId == 0x00)

        val scriptAddr = ShelleyAddress(
          Network.Mainnet,
          ShelleyPaymentPart.scriptHash(sampleScriptHash),
          ShelleyDelegationPart.keyHash(sampleStakeHash)
        )
        assert(scriptAddr.typeId == 0x01)

        val enterpriseAddr = ShelleyAddress(
          Network.Mainnet,
          payment,
          ShelleyDelegationPart.Null
        )
        assert(enterpriseAddr.typeId == 0x06)
        assert(enterpriseAddr.isEnterprise == true)
    }

    test("ShelleyAddress should build correct headers") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val mainnetAddr = ShelleyAddress(Network.Mainnet, payment, delegation)
        val testnetAddr = ShelleyAddress(Network.Testnet, payment, delegation)

        assert(mainnetAddr.toHeader == 0x01) // type 0 (0000) + mainnet (0001)
        assert(testnetAddr.toHeader == 0x00) // type 0 (0000) + testnet (0000)
    }

    test("StakeAddress should calculate correct type IDs and headers") {
        val stakeAddr = StakeAddress(Network.Mainnet, StakePayload.stakeKey(sampleStakeHash))
        val scriptStakeAddr = StakeAddress(Network.Mainnet, StakePayload.script(sampleScriptHash))

        assert(stakeAddr.typeId == 0x0e)
        assert(scriptStakeAddr.typeId == 0x0f)

        assert(stakeAddr.toHeader == 0xe1.toByte) // type 14 (1110) + mainnet (0001)
        assert(scriptStakeAddr.toHeader == 0xf1.toByte) // type 15 (1111) + mainnet (0001)

        assert(stakeAddr.isScript == false)
        assert(scriptStakeAddr.isScript == true)
    }

    test("Address serialization should produce correct bytes") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val shelleyAddr = ShelleyAddress(Network.Mainnet, payment, delegation)

        val bytes = shelleyAddr.toBytes
        assert(bytes.length == 57) // 1 header + 28 payment + 28 delegation
        assert(bytes.bytes(0) == 0x01) // header

        val stakeAddr = StakeAddress(Network.Mainnet, StakePayload.stakeKey(sampleStakeHash))
        val stakeBytes = stakeAddr.toBytes
        assert(stakeBytes.length == 29) // 1 header + 28 hash
        assert(stakeBytes.bytes(0) == 0xe1.toByte)
    }

    test("Address parsing should handle all address types") {
        // Test each address type parser with manually constructed bytes

        // Type 0: Payment Key + Stake Key
        val type0Header: Byte = 0x01 // type 0 + mainnet
        val type0Payload = samplePaymentHash.hash.bytes.bytes ++ sampleStakeHash.bytes.bytes
        val type0Bytes = type0Header +: type0Payload

        val parsedType0 = fromBytes(type0Bytes)
        parsedType0 match {
            case Address.Shelley(_) => // Expected
            case _                  => fail("Expected Shelley address")
        }
        assert(parsedType0.typeId == 0)

        // Type 14: Stake Key
        val type14Header: Byte = 0xe1.toByte // type 14 + mainnet
        val type14Bytes = type14Header +: sampleStakeHash.bytes.bytes

        val parsedType14 = fromBytes(type14Bytes)

        parsedType14 match {
            case Address.Stake(_) => // Expected
            case _                => fail("Expected Stake address")
        }
        assert(parsedType14.typeId == 14)
    }

    test("Address parsing should handle invalid inputs gracefully") {
        // Empty bytes
        assertThrows[IllegalArgumentException] {
            fromBytes(Array.empty)
        }

        // Invalid header type
        val invalidHeader: Byte = 0x91.toByte // type 9 doesn't exist
        val invalidBytes = invalidHeader +: samplePaymentHash.hash.bytes.bytes
        assertThrows[IllegalArgumentException] {
            fromBytes(invalidBytes)
        }

        // Wrong payload length
        val shortPayload = Array[Byte](0x01, 0x02, 0x03) // too short
        assertThrows[IllegalArgumentException] {
            fromBytes(shortPayload)
        }
    }

    test("Address round-trip should preserve data") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val originalAddr = ShelleyAddress(Network.Mainnet, payment, delegation)
        val wrappedAddr = Address.Shelley(originalAddr)

        // Serialize and parse back
        val bytes = wrappedAddr.toBytes.bytes
        val parsedAddr = fromBytes(bytes)

        assert(parsedAddr == wrappedAddr)
        assert(parsedAddr.typeId == wrappedAddr.typeId)
        assert(parsedAddr.network == wrappedAddr.network)
    }

    test("Address utility methods should work correctly") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val scriptPayment = ShelleyPaymentPart.scriptHash(sampleScriptHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val normalAddr = Address.Shelley(ShelleyAddress(Network.Mainnet, payment, delegation))
        val scriptAddr = Address.Shelley(ShelleyAddress(Network.Mainnet, scriptPayment, delegation))
        val enterpriseAddr =
            Address.Shelley(ShelleyAddress(Network.Mainnet, payment, ShelleyDelegationPart.Null))
        val stakeAddr =
            Address.Stake(StakeAddress(Network.Mainnet, StakePayload.stakeKey(sampleStakeHash)))

        assert(normalAddr.hasScript == false)
        assert(scriptAddr.hasScript == true)

        assert(normalAddr.isEnterprise == false)
        assert(enterpriseAddr.isEnterprise == true)
        assert(stakeAddr.isEnterprise == false)

        assert(normalAddr.network == Some(Network.Mainnet))
        assert(stakeAddr.network == Some(Network.Mainnet))
    }

    test("Address conversion between Shelley and Stake should work") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)
        val shelleyAddr = ShelleyAddress(Network.Mainnet, payment, delegation)

        val stakeAddr = Address.shelleyToStake(shelleyAddr)
        assert(stakeAddr.isSuccess)

        val convertedStake = stakeAddr.get
        assert(convertedStake.network == shelleyAddr.network)
        assert(convertedStake.payload.asHash == delegation.asHash.get)

        // Test conversion failure for enterprise address
        val enterpriseAddr = ShelleyAddress(Network.Mainnet, payment, ShelleyDelegationPart.Null)
        val enterpriseConversion = Address.shelleyToStake(enterpriseAddr)
        assert(enterpriseConversion.isFailure)
    }

    test("Edge cases should be handled correctly") {
        // Test maximum values for variable length encoding
        val maxSlot = Long.MaxValue
        val maxPointer = Pointer(Slot(maxSlot), Long.MaxValue, Long.MaxValue)
        val encoded = maxPointer.toBytes
        assert(encoded.length > 21) // Will use multiple bytes per field

        val decoded = Pointer.fromBytes(encoded)
        assert(decoded == Success(maxPointer))

        // Test zero values
        val minPointer = Pointer(Slot(0), 0, 0)
        val minEncoded = minPointer.toBytes
        assert(minEncoded.sameElements(Array[Byte](0x00, 0x00, 0x00)))

        val minDecoded = Pointer.fromBytes(minEncoded)
        assert(minDecoded == Success(minPointer))
    }

    test("Human readable prefixes should be correct") {
        val payment = ShelleyPaymentPart.keyHash(samplePaymentHash)
        val delegation = ShelleyDelegationPart.keyHash(sampleStakeHash)

        val mainnetShelley = ShelleyAddress(Network.Mainnet, payment, delegation)
        val testnetShelley = ShelleyAddress(Network.Testnet, payment, delegation)

        assert(mainnetShelley.hrp == Success("addr"))
        assert(testnetShelley.hrp == Success("addr_test"))

        val mainnetStake = StakeAddress(Network.Mainnet, StakePayload.stakeKey(sampleStakeHash))
        val testnetStake = StakeAddress(Network.Testnet, StakePayload.stakeKey(sampleStakeHash))

        assert(mainnetStake.hrp == Success("stake"))
        assert(testnetStake.hrp == Success("stake_test"))

        // Test unknown network
        val unknownShelley = ShelleyAddress(Network.Other(0x05), payment, delegation)
        assert(unknownShelley.hrp.isFailure)
    }
}
