package scalus.ledger

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Success, Try}
import scalus.builtin.ByteString
import scalus.utils.Utils

/** Test suite for CardanoAddress implementation Tests cover address encoding/decoding for different
  * address types based on CIP-19 test vectors
  */
class CardanoAddressSpec extends AnyFunSuite {
    import CardanoAddress.*

    // Test vectors from CIP-19
    private val testVectors = Map(
      // Mainnet addresses
      "type-00-mainnet" -> "addr1qx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgse35a3x",
      "type-01-mainnet" -> "addr1z8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs9yc0hh",
      "type-02-mainnet" -> "addr1yx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzerkr0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shs2z78ve",
      "type-03-mainnet" -> "addr1x8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gt7r0vd4msrxnuwnccdxlhdjar77j6lg0wypcc9uar5d2shskhj42g",
      "type-04-mainnet" -> "addr1gx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer5pnz75xxcrzqf96k",
      "type-05-mainnet" -> "addr128phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtupnz75xxcrtw79hu",
      "type-06-mainnet" -> "addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8",
      "type-07-mainnet" -> "addr1w8phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcyjy7wx",
      "type-14-mainnet" -> "stake1uyehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gh6ffgw",
      "type-15-mainnet" -> "stake178phkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcccycj5",

      // Testnet addresses
      "type-00-testnet" -> "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3n0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgs68faae",
      "type-01-testnet" -> "addr_test1zrphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gten0d3vllmyqwsx5wktcd8cc3sq835lu7drv2xwl2wywfgsxj90mg",
      "type-14-testnet" -> "stake_test1uqehkck0lajq8gr28t9uxnuvgcqrc6070x3k9r8048z8y5gssrtvn",
      "type-15-testnet" -> "stake_test17rphkx6acpnf78fuvxn0mkew3l0fd058hzquvz7w36x4gtcljw6kf"
    )

    // Sample hashes from test vectors (these are example values to use in tests)
    private val paymentKeyHashHex =
        val bytes = Bech32.decode("addr_vk1w0l2sr2zgfm26ztc6nl9xy8ghsk5sh6ldwemlpmp9xylzy4dtf7st80zhd").get._2
        Utils.bytesToHex(bytes)
    private val stakeKeyHashHex = "1d227aefa4b773149170885aadba30aab3127cc611ddbc4999def61c"
    private val scriptHashHex = "2a25e6a9c3c5047303e80938d17c5f2c340ba0e3726b42f3e71c6b0"

    /** Test case for Type-0 address (PaymentKeyHash with StakeKeyHash)
      */
    test("correctly encode and decode Type-0 addresses") {
        // Create hashes from hex strings
        val paymentKeyHash = Hash28.fromHex(paymentKeyHashHex)
        val stakeKeyHash = Hash28.fromHex(stakeKeyHashHex)

        // Create address for mainnet
        val mainnetAddress = AddressType.PaymentKeyHashWithStakeKeyHash(
          paymentKeyHash,
          stakeKeyHash,
          MainnetTag
        )

        // Encode the address
        val encodedMainnet = CardanoAddress.encode(mainnetAddress).get

        // Expected mainnet address from test vector
        val expectedMainnet = testVectors("type-00-mainnet")

        // Verify encoded address matches expected
        assert(encodedMainnet == expectedMainnet)

        // Test decoding
        val decodedMainnet = CardanoAddress.decode(encodedMainnet).get

        // Verify decoded address matches original
        assert(decodedMainnet == mainnetAddress)

        // Create address for testnet
        val testnetAddress = AddressType.PaymentKeyHashWithStakeKeyHash(
          paymentKeyHash,
          stakeKeyHash,
          TestnetTag
        )

        // Encode the testnet address
        val encodedTestnet = CardanoAddress.encode(testnetAddress).get

        // Expected testnet address from test vector
        val expectedTestnet = testVectors("type-00-testnet")

        // Verify encoded testnet address matches expected
        assert(encodedTestnet == expectedTestnet)

        // Test decoding of testnet address
        val decodedTestnet = CardanoAddress.decode(encodedTestnet).get

        // Verify decoded testnet address matches original
        assert(decodedTestnet == testnetAddress)
    }

    /** Test case for Type-1 address (ScriptHash with StakeKeyHash)
      */
    test("correctly encode and decode Type-1 addresses") {
        // Create hashes from hex strings
        val scriptHash = Hash28.fromHex(scriptHashHex)
        val stakeKeyHash = Hash28.fromHex(stakeKeyHashHex)

        // Create address for mainnet
        val mainnetAddress = AddressType.ScriptHashWithStakeKeyHash(
          scriptHash,
          stakeKeyHash,
          MainnetTag
        )

        // Encode the address
        val encodedMainnet = CardanoAddress.encode(mainnetAddress).get

        // Expected mainnet address from test vector
        val expectedMainnet = testVectors("type-01-mainnet")

        // Verify encoded address matches expected
        assert(encodedMainnet == expectedMainnet)

        // Test decoding
        val decodedMainnet = CardanoAddress.decode(encodedMainnet).get

        // Verify decoded address matches original
        assert(decodedMainnet == mainnetAddress)

        // Test for testnet
        val testnetAddress = AddressType.ScriptHashWithStakeKeyHash(
          scriptHash,
          stakeKeyHash,
          TestnetTag
        )

        val encodedTestnet = CardanoAddress.encode(testnetAddress).get
        val expectedTestnet = testVectors("type-01-testnet")

        assert(encodedTestnet == expectedTestnet)

        val decodedTestnet = CardanoAddress.decode(encodedTestnet).get
        assert(decodedTestnet == testnetAddress)
    }

    /** Test case for Type-14 address (Stake Key Hash)
      */
    test("correctly encode and decode Type-14 stake addresses") {
        // Create stake key hash from hex string
        val stakeKeyHash = Hash28.fromHex(stakeKeyHashHex)

        // Create stake address for mainnet
        val mainnetAddress = AddressType.StakeKeyHash(
          stakeKeyHash,
          MainnetTag
        )

        // Encode the address
        val encodedMainnet = CardanoAddress.encode(mainnetAddress).get

        // Expected mainnet stake address from test vector
        val expectedMainnet = testVectors("type-14-mainnet")

        // Verify encoded address matches expected
        assert(encodedMainnet == expectedMainnet)

        // Test decoding
        val decodedMainnet = CardanoAddress.decode(encodedMainnet).get

        // Verify decoded address matches original
        assert(decodedMainnet == mainnetAddress)

        // Test for testnet
        val testnetAddress = AddressType.StakeKeyHash(
          stakeKeyHash,
          TestnetTag
        )

        val encodedTestnet = CardanoAddress.encode(testnetAddress).get
        val expectedTestnet = testVectors("type-14-testnet")

        assert(encodedTestnet == expectedTestnet)

        val decodedTestnet = CardanoAddress.decode(encodedTestnet).get
        assert(decodedTestnet == testnetAddress)
    }

    /** Test case for Type-15 address (Stake Script Hash)
      */
    test("correctly encode and decode Type-15 stake script addresses") {
        // Create script hash from hex string
        val scriptHash = Hash28.fromHex(scriptHashHex)

        // Create stake script address for mainnet
        val mainnetAddress = AddressType.StakeScriptHash(
          scriptHash,
          MainnetTag
        )

        // Encode the address
        val encodedMainnet = CardanoAddress.encode(mainnetAddress).get

        // Expected mainnet stake script address from test vector
        val expectedMainnet = testVectors("type-15-mainnet")

        // Verify encoded address matches expected
        assert(encodedMainnet == expectedMainnet)

        // Test decoding
        val decodedMainnet = CardanoAddress.decode(encodedMainnet).get

        // Verify decoded address matches original
        assert(decodedMainnet == mainnetAddress)

        // Test for testnet
        val testnetAddress = AddressType.StakeScriptHash(
          scriptHash,
          TestnetTag
        )

        val encodedTestnet = CardanoAddress.encode(testnetAddress).get
        val expectedTestnet = testVectors("type-15-testnet")

        assert(encodedTestnet == expectedTestnet)

        val decodedTestnet = CardanoAddress.decode(encodedTestnet).get
        assert(decodedTestnet == testnetAddress)
    }

    /** Test case for pointer addresses (Type-4)
      */
    test("correctly handle pointer addresses") {
        // Create payment key hash from hex string
        val paymentKeyHash = Hash28.fromHex(paymentKeyHashHex)

        // Create pointer from test vector (2498243, 27, 3)
        val pointer = Pointer(2498243, 27, 3)

        // Create pointer address for mainnet
        val address = AddressType.PaymentKeyHashWithPointer(
          paymentKeyHash,
          pointer,
          MainnetTag
        )

        // Test variable-length pointer encoding/decoding
        val bytes = serializeAddress(address).bytes
        val headerByte = bytes(0)
        val extractedType = (headerByte >> 4) & 0x0f
        val extractedTag = headerByte & 0x0f

        // Verify header type and network tag
        assert(extractedType == HeaderType.PaymentKeyHashWithPointer)
        assert(extractedTag == MainnetTag)

        // Decode the address
        val pointerBytes = bytes.slice(1 + Hash28.Size, bytes.length)
        val (decodedPointer, _) = decodePointer(pointerBytes, 0)

        // Verify decoded pointer matches original
        assert(decodedPointer.slot == pointer.slot)
        assert(decodedPointer.txIndex == pointer.txIndex)
        assert(decodedPointer.certIndex == pointer.certIndex)
    }

    /** Test case for variable-length integer encoding/decoding
      */
    test("correctly encode and decode variable-length integers") {
        // Test small value (fits in one byte)
        val smallValue = 42L
        val smallEncoded = encodeVariableLengthUInt(smallValue)
        assert(smallEncoded.length == 1)
        assert(smallEncoded(0) == 42.toByte)

        val (decodedSmall, bytesUsedSmall) = decodeVariableLengthUInt(smallEncoded, 0)
        assert(decodedSmall == smallValue)
        assert(bytesUsedSmall == 1)

        // Test medium value (two bytes)
        val mediumValue = 130L // Just over 127 to use two bytes
        val mediumEncoded = encodeVariableLengthUInt(mediumValue)
        assert(mediumEncoded.length == 2)

        val (decodedMedium, bytesUsedMedium) = decodeVariableLengthUInt(mediumEncoded, 0)
        assert(decodedMedium == mediumValue)
        assert(bytesUsedMedium == 2)

        // Test large value
        val largeValue = 2498243L // Value from test vector
        val largeEncoded = encodeVariableLengthUInt(largeValue)

        val (decodedLarge, bytesUsedLarge) = decodeVariableLengthUInt(largeEncoded, 0)
        assert(decodedLarge == largeValue)
    }

    /** Test edge cases and error handling
      */
    test("handle edge cases properly") {
        // Test with malformed Bech32 address
        val invalidAddress = "addr1badaddress"
        assert(CardanoAddress.decode(invalidAddress).isFailure)

        // Test with negative value for variable-length integer encoding
        intercept[IllegalArgumentException] {
            encodeVariableLengthUInt(-1)
        }

        // Test with invalid header type
        val invalidHeaderType = 0x0b // Not defined in the standard
        val invalidHeader = ((invalidHeaderType << 4) | (MainnetTag & 0x0f)).toByte
        val paymentKeyHash = Hash28.fromHex(paymentKeyHashHex)
        val invalidBytes = ByteString(invalidHeader) ++ paymentKeyHash.bytes

        val invalidBech32 = Bech32.encodeFrom5Bit("addr", Bech32.to5Bit(invalidBytes.bytes)).get

        // Should fail with unsupported header type
        assert(CardanoAddress.decode(invalidBech32).isFailure)
    }
}
