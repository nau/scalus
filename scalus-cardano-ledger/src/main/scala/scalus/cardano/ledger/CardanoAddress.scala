package scalus.cardano.ledger

import scala.util.{Failure, Success, Try}
import scalus.builtin.ByteString

/** Implementation of Cardano CIP-19 address format. Handles the binary structure of Cardano
  * addresses including Shelley, Stake, and Byron addresses. Uses strongly typed Hash28 classes for
  * payment, stake and script hashes.
  */
object CardanoAddress {
    // Network tag constants
    val TestnetTag: Byte = 0x00
    val MainnetTag: Byte = 0x01

    // Address header type constants (bits 7-4 in header byte)
    object HeaderType {
        // Shelley addresses
        val PaymentKeyHashWithStakeKeyHash: Byte = 0x00
        val ScriptHashWithStakeKeyHash: Byte = 0x01
        val PaymentKeyHashWithScriptHash: Byte = 0x02
        val ScriptHashWithScriptHash: Byte = 0x03
        val PaymentKeyHashWithPointer: Byte = 0x04
        val ScriptHashWithPointer: Byte = 0x05
        val PaymentKeyHashOnly: Byte = 0x06
        val ScriptHashOnly: Byte = 0x07

        // Byron addresses
        val Byron: Byte = 0x08

        // Stake addresses
        val StakeKeyHash: Byte = 0x0e
        val StakeScriptHash: Byte = 0x0f
    }

    // Byron address minimal size
    val ByronAddressMinSize: Int = 29 // Minimum size of a Byron address (header + minimal payload)

    /** Represents a pointer to a stake registration certificate on the blockchain
      *
      * @param slot
      *   Transaction slot number
      * @param txIndex
      *   Transaction index within the slot
      * @param certIndex
      *   Certificate index within the transaction
      */
    case class Pointer(slot: Long, txIndex: Int, certIndex: Int)

    /** Represents the different address types in Cardano
      */
    enum AddressType {
        // Shelley address types
        case PaymentKeyHashWithStakeKeyHash(
            paymentKeyHash: Hash28,
            stakeKeyHash: Hash28,
            networkTag: Byte
        )
        case ScriptHashWithStakeKeyHash(scriptHash: Hash28, stakeKeyHash: Hash28, networkTag: Byte)
        case PaymentKeyHashWithScriptHash(
            paymentKeyHash: Hash28,
            scriptHash: Hash28,
            networkTag: Byte
        )
        case ScriptHashWithScriptHash(scriptHash1: Hash28, scriptHash2: Hash28, networkTag: Byte)
        case PaymentKeyHashWithPointer(paymentKeyHash: Hash28, pointer: Pointer, networkTag: Byte)
        case ScriptHashWithPointer(scriptHash: Hash28, pointer: Pointer, networkTag: Byte)
        case PaymentKeyHashOnly(paymentKeyHash: Hash28, networkTag: Byte)
        case ScriptHashOnly(scriptHash: Hash28, networkTag: Byte)

        // Byron address type - stored as ByteString since it has a complex internal structure
        case Byron(addressBytes: ByteString)

        // Stake address types
        case StakeKeyHash(stakeKeyHash: Hash28, networkTag: Byte)
        case StakeScriptHash(scriptHash: Hash28, networkTag: Byte)
    }

    /** Encodes a Cardano address to its canonical string representation
      *
      * @param addressType
      *   The Cardano address to encode
      * @return
      *   Success with encoded address string or Failure with exception
      */
    def encode(addressType: AddressType): Try[String] = Try {
        val bytes = serializeAddress(addressType)

        addressType match {
            case AddressType.Byron(rawBytes) =>
                // Byron addresses are encoded as Base58
                // Not implemented in this version as we're focusing on Shelley addresses
                throw new UnsupportedOperationException(
                  "Byron address encoding not implemented yet"
                )

            case _ =>
                // All other address types use Bech32
                val hrp = determineHrpPrefix(addressType)
                Bech32.encodeFrom5Bit(hrp, Bech32.to5Bit(bytes.bytes))
        }
    }

    /** Determines the human-readable prefix for Bech32 encoding based on address type
      *
      * @param addressType
      *   The address type to get prefix for
      * @return
      *   The appropriate hrp prefix
      */
    private def determineHrpPrefix(addressType: AddressType): String = {
        // Extract network tag when available
        val isTestnet = addressType match {
            case AddressType.Byron(_) => false // Default to mainnet for Byron addresses
            case AddressType.PaymentKeyHashWithStakeKeyHash(_, _, tag) => tag == TestnetTag
            case AddressType.ScriptHashWithStakeKeyHash(_, _, tag)     => tag == TestnetTag
            case AddressType.PaymentKeyHashWithScriptHash(_, _, tag)   => tag == TestnetTag
            case AddressType.ScriptHashWithScriptHash(_, _, tag)       => tag == TestnetTag
            case AddressType.PaymentKeyHashWithPointer(_, _, tag)      => tag == TestnetTag
            case AddressType.ScriptHashWithPointer(_, _, tag)          => tag == TestnetTag
            case AddressType.PaymentKeyHashOnly(_, tag)                => tag == TestnetTag
            case AddressType.ScriptHashOnly(_, tag)                    => tag == TestnetTag
            case AddressType.StakeKeyHash(_, tag)                      => tag == TestnetTag
            case AddressType.StakeScriptHash(_, tag)                   => tag == TestnetTag
        }

        // Determine if it's a stake address
        val isStakeAddress = addressType match {
            case AddressType.StakeKeyHash(_, _)    => true
            case AddressType.StakeScriptHash(_, _) => true
            case _                                 => false
        }

        // Return appropriate prefix based on network and address type
        if isStakeAddress then {
            if isTestnet then "stake_test" else "stake"
        } else {
            if isTestnet then "addr_test" else "addr"
        }
    }

    /** Serializes an address type to its binary representation
      *
      * @param addressType
      *   The address type to serialize
      * @return
      *   ByteString representing the serialized address
      */
    def serializeAddress(addressType: AddressType): ByteString = {
        addressType match {
            case AddressType.Byron(rawBytes) =>
                // Byron addresses are already in serialized form
                rawBytes

            case AddressType.PaymentKeyHashWithStakeKeyHash(
                  paymentKeyHash,
                  stakeKeyHash,
                  networkTag
                ) =>
                serializeShelleyAddress(
                  HeaderType.PaymentKeyHashWithStakeKeyHash,
                  networkTag,
                  paymentKeyHash.bytes,
                  stakeKeyHash.bytes
                )

            case AddressType.ScriptHashWithStakeKeyHash(scriptHash, stakeKeyHash, networkTag) =>
                serializeShelleyAddress(
                  HeaderType.ScriptHashWithStakeKeyHash,
                  networkTag,
                  scriptHash.bytes,
                  stakeKeyHash.bytes
                )

            case AddressType.PaymentKeyHashWithScriptHash(paymentKeyHash, scriptHash, networkTag) =>
                serializeShelleyAddress(
                  HeaderType.PaymentKeyHashWithScriptHash,
                  networkTag,
                  paymentKeyHash.bytes,
                  scriptHash.bytes
                )

            case AddressType.ScriptHashWithScriptHash(scriptHash1, scriptHash2, networkTag) =>
                serializeShelleyAddress(
                  HeaderType.ScriptHashWithScriptHash,
                  networkTag,
                  scriptHash1.bytes,
                  scriptHash2.bytes
                )

            case AddressType.PaymentKeyHashWithPointer(paymentKeyHash, pointer, networkTag) =>
                serializeShelleyAddressWithPointer(
                  HeaderType.PaymentKeyHashWithPointer,
                  networkTag,
                  paymentKeyHash.bytes,
                  pointer
                )

            case AddressType.ScriptHashWithPointer(scriptHash, pointer, networkTag) =>
                serializeShelleyAddressWithPointer(
                  HeaderType.ScriptHashWithPointer,
                  networkTag,
                  scriptHash.bytes,
                  pointer
                )

            case AddressType.PaymentKeyHashOnly(paymentKeyHash, networkTag) =>
                serializeShelleyAddressBasic(
                  HeaderType.PaymentKeyHashOnly,
                  networkTag,
                  paymentKeyHash.bytes
                )

            case AddressType.ScriptHashOnly(scriptHash, networkTag) =>
                serializeShelleyAddressBasic(
                  HeaderType.ScriptHashOnly,
                  networkTag,
                  scriptHash.bytes
                )

            case AddressType.StakeKeyHash(stakeKeyHash, networkTag) =>
                serializeStakeAddress(HeaderType.StakeKeyHash, networkTag, stakeKeyHash.bytes)

            case AddressType.StakeScriptHash(scriptHash, networkTag) =>
                serializeStakeAddress(HeaderType.StakeScriptHash, networkTag, scriptHash.bytes)
        }
    }

    /** Serializes a Shelley address with payment and delegation parts
      *
      * @param headerType
      *   The header type nibble (bits 7-4)
      * @param networkTag
      *   The network tag nibble (bits 3-0)
      * @param part1
      *   The payment part (key hash or script hash)
      * @param part2
      *   The delegation part (key hash or script hash)
      * @return
      *   Serialized address as ByteString
      */
    private def serializeShelleyAddress(
        headerType: Byte,
        networkTag: Byte,
        part1: ByteString,
        part2: ByteString
    ): ByteString = {
        val header = ((headerType << 4) | (networkTag & 0x0f)).toByte
        ByteString(header) ++ part1 ++ part2
    }

    /** Serializes a Shelley address with pointer
      *
      * @param headerType
      *   The header type nibble (bits 7-4)
      * @param networkTag
      *   The network tag nibble (bits 3-0)
      * @param paymentPart
      *   The payment part (key hash or script hash)
      * @param pointer
      *   The stake pointer
      * @return
      *   Serialized address as ByteString
      */
    private def serializeShelleyAddressWithPointer(
        headerType: Byte,
        networkTag: Byte,
        paymentPart: ByteString,
        pointer: Pointer
    ): ByteString = {
        val header = ((headerType << 4) | (networkTag & 0x0f)).toByte
        val pointerBytes = encodePointer(pointer)

        ByteString(header) ++ paymentPart ++ ByteString.unsafeFromArray(pointerBytes)
    }

    /** Encodes a pointer using variable-length encoding
      *
      * @param pointer
      *   The pointer to encode
      * @return
      *   Encoded pointer bytes
      */
    private def encodePointer(pointer: Pointer): Array[Byte] = {
        encodeVariableLengthUInt(pointer.slot) ++
            encodeVariableLengthUInt(pointer.txIndex) ++
            encodeVariableLengthUInt(pointer.certIndex)
    }

    /** Encodes a positive integer using variable-length encoding as specified in CIP-19
      *
      * @param value
      *   The positive integer to encode
      * @return
      *   Encoded bytes
      */
    private[ledger] def encodeVariableLengthUInt(value: Long): Array[Byte] = {
        require(value >= 0, "Value must be non-negative")

        var v = value
        val buffer = scala.collection.mutable.ArrayBuffer.empty[Byte]

        while
            var byte = (v & 0x7f).toByte
            v >>>= 7
            if v != 0 then byte = (byte | 0x80).toByte // set continuation bit
            buffer += byte
            v != 0
        do ()

        buffer.toArray
    }

    /** Serializes a Shelley address with only payment part
      *
      * @param headerType
      *   The header type nibble (bits 7-4)
      * @param networkTag
      *   The network tag nibble (bits 3-0)
      * @param paymentPart
      *   The payment part (key hash or script hash)
      * @return
      *   Serialized address as ByteString
      */
    private def serializeShelleyAddressBasic(
        headerType: Byte,
        networkTag: Byte,
        paymentPart: ByteString
    ): ByteString = {
        val header = ((headerType << 4) | (networkTag & 0x0f)).toByte
        ByteString(header) ++ paymentPart
    }

    /** Serializes a stake address
      *
      * @param headerType
      *   The header type nibble (bits 7-4)
      * @param networkTag
      *   The network tag nibble (bits 3-0)
      * @param stakePart
      *   The stake part (key hash or script hash)
      * @return
      *   Serialized address as ByteString
      */
    private def serializeStakeAddress(
        headerType: Byte,
        networkTag: Byte,
        stakePart: ByteString
    ): ByteString = {
        val header = ((headerType << 4) | (networkTag & 0x0f)).toByte
        ByteString(header) ++ stakePart
    }

    /** Decodes a Cardano address from its string representation
      *
      * @param address
      *   The address string to decode
      * @return
      *   Success with address type or Failure with exception
      */
    def decode(address: String): Try[AddressType] = {
        // Check if it's a Bech32 address (Shelley or stake) or Base58 (Byron)
        if address.contains('1') then {
            // It's likely a Bech32 address
            decodeBech32Address(address)
        } else {
            // It's likely a Base58 address (Byron)
            decodeByronAddress(address)
        }
    }

    /** Decodes a Bech32-encoded Cardano address
      *
      * @param address
      *   The Bech32 address string
      * @return
      *   Success with address type or Failure with exception
      */
    private def decodeBech32Address(address: String): Try[AddressType] = Try {
        val decoded = Bech32.decode(address)
        val bytes = ByteString.fromArray(decoded.data)
        // Extract header byte
        require(bytes.length > 0, "Address is too short")
        val header = bytes.bytes(0)

        // Extract header type (bits 7-4) and network tag (bits 3-0)
        val headerType = (header >> 4) & 0x0f
        val networkTag = header & 0x0f

        // Parse according to header type
        headerType match {
            case 0x00 =>
                parseType0(
                  bytes,
                  networkTag.toByte
                ) // Payment Key Hash with Stake Key Hash
            case 0x01 =>
                parseType1(bytes, networkTag.toByte) // Script Hash with Stake Key Hash
            case 0x02 =>
                parseType2(
                  bytes,
                  networkTag.toByte
                ) // Payment Key Hash with Script Hash
            case 0x03 =>
                parseType3(bytes, networkTag.toByte) // Script Hash with Script Hash
            case 0x04 =>
                parseType4(bytes, networkTag.toByte) // Payment Key Hash with Pointer
            case 0x05 =>
                parseType5(bytes, networkTag.toByte) // Script Hash with Pointer
            case 0x06 => parseType6(bytes, networkTag.toByte) // Payment Key Hash Only
            case 0x07 => parseType7(bytes, networkTag.toByte) // Script Hash Only
            case 0x0e => parseType14(bytes, networkTag.toByte) // Stake Key Hash
            case 0x0f => parseType15(bytes, networkTag.toByte) // Stake Script Hash
            case _ =>
                throw new IllegalArgumentException(
                  s"Unsupported header type: $headerType"
                )
        }
    }

    /** Parses a Type-0 address (Payment Key Hash with Stake Key Hash)
      */
    private def parseType0(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size * 2,
          s"Invalid Type-0 address length: ${bytes.length}, expected ${1 + Hash28.Size * 2}"
        )

        val paymentKeyHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size))
        )
        val stakeKeyHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1 + Hash28.Size, 1 + Hash28.Size * 2))
        )

        AddressType.PaymentKeyHashWithStakeKeyHash(paymentKeyHash, stakeKeyHash, networkTag)
    }

    /** Parses a Type-1 address (Script Hash with Stake Key Hash)
      */
    private def parseType1(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size * 2,
          s"Invalid Type-1 address length: ${bytes.length}, expected ${1 + Hash28.Size * 2}"
        )

        val scriptHash = Hash28(ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size)))
        val stakeKeyHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1 + Hash28.Size, 1 + Hash28.Size * 2))
        )

        AddressType.ScriptHashWithStakeKeyHash(scriptHash, stakeKeyHash, networkTag)
    }

    /** Parses a Type-2 address (Payment Key Hash with Script Hash)
      */
    private def parseType2(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size * 2,
          s"Invalid Type-2 address length: ${bytes.length}, expected ${1 + Hash28.Size * 2}"
        )

        val paymentKeyHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size))
        )
        val scriptHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1 + Hash28.Size, 1 + Hash28.Size * 2))
        )

        AddressType.PaymentKeyHashWithScriptHash(paymentKeyHash, scriptHash, networkTag)
    }

    /** Parses a Type-3 address (Script Hash with Script Hash)
      */
    private def parseType3(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size * 2,
          s"Invalid Type-3 address length: ${bytes.length}, expected ${1 + Hash28.Size * 2}"
        )

        val scriptHash1 = Hash28(ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size)))
        val scriptHash2 = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1 + Hash28.Size, 1 + Hash28.Size * 2))
        )

        AddressType.ScriptHashWithScriptHash(scriptHash1, scriptHash2, networkTag)
    }

    /** Parses a Type-4 address (Payment Key Hash with Pointer)
      */
    private def parseType4(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length > 1 + Hash28.Size,
          s"Invalid Type-4 address length: ${bytes.length}, expected > ${1 + Hash28.Size}"
        )

        val paymentKeyHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size))
        )
        val pointerBytes = bytes.bytes.slice(1 + Hash28.Size, bytes.length.toInt)

        val (pointer, _) = decodePointer(pointerBytes, 0)

        AddressType.PaymentKeyHashWithPointer(paymentKeyHash, pointer, networkTag)
    }

    /** Parses a Type-5 address (Script Hash with Pointer)
      */
    private def parseType5(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length > 1 + Hash28.Size,
          s"Invalid Type-5 address length: ${bytes.length}, expected > ${1 + Hash28.Size}"
        )

        val scriptHash = Hash28(ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size)))
        val pointerBytes = bytes.bytes.slice(1 + Hash28.Size, bytes.length.toInt)

        val (pointer, _) = decodePointer(pointerBytes, 0)

        AddressType.ScriptHashWithPointer(scriptHash, pointer, networkTag)
    }

    /** Parses a Type-6 address (Payment Key Hash Only)
      */
    private def parseType6(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size,
          s"Invalid Type-6 address length: ${bytes.length}, expected ${1 + Hash28.Size}"
        )

        val paymentKeyHash = Hash28(
          ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size))
        )

        AddressType.PaymentKeyHashOnly(paymentKeyHash, networkTag)
    }

    /** Parses a Type-7 address (Script Hash Only)
      */
    private def parseType7(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size,
          s"Invalid Type-7 address length: ${bytes.length}, expected ${1 + Hash28.Size}"
        )

        val scriptHash = Hash28(ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size)))

        AddressType.ScriptHashOnly(scriptHash, networkTag)
    }

    /** Parses a Type-14 address (Stake Key Hash)
      */
    private def parseType14(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size,
          s"Invalid Type-14 address length: ${bytes.length}, expected ${1 + Hash28.Size}"
        )

        val stakeKeyHash = Hash28(ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size)))

        AddressType.StakeKeyHash(stakeKeyHash, networkTag)
    }

    /** Parses a Type-15 address (Stake Script Hash)
      */
    private def parseType15(bytes: ByteString, networkTag: Byte): AddressType = {
        require(
          bytes.length == 1 + Hash28.Size,
          s"Invalid Type-15 address length: ${bytes.length}, expected ${1 + Hash28.Size}"
        )

        val scriptHash = Hash28(ByteString.unsafeFromArray(bytes.bytes.slice(1, 1 + Hash28.Size)))

        AddressType.StakeScriptHash(scriptHash, networkTag)
    }

    /** Decodes a variable-length pointer from byte array
      *
      * @param bytes
      *   The bytes containing the pointer
      * @param startIndex
      *   The index to start decoding from
      * @return
      *   Tuple of (decoded pointer, number of bytes consumed)
      */
    private[ledger] def decodePointer(bytes: Array[Byte], startIndex: Int): (Pointer, Int) = {
        val (slot, bytesUsed1) = decodeVariableLengthUInt(bytes, startIndex)
        val (txIndex, bytesUsed2) = decodeVariableLengthUInt(bytes, startIndex + bytesUsed1)
        val (certIndex, bytesUsed3) =
            decodeVariableLengthUInt(bytes, startIndex + bytesUsed1 + bytesUsed2)

        (Pointer(slot, txIndex.toInt, certIndex.toInt), bytesUsed1 + bytesUsed2 + bytesUsed3)
    }

    /** Decodes a variable-length integer as specified in CIP-19
      *
      * @param bytes
      *   The bytes containing the encoded integer
      * @param startIndex
      *   The index to start decoding from
      * @return
      *   Tuple of (decoded value, number of bytes consumed)
      */
    private[ledger] def decodeVariableLengthUInt(
        bytes: Array[Byte],
        startIndex: Int
    ): (Long, Int) = {
        require(startIndex < bytes.length, "Start index out of bounds")

        val firstByte = bytes(startIndex)
        val hasMoreBytes = (firstByte & 0x80) != 0

        if !hasMoreBytes then {
            // Simple case: single byte with MSB=0
            (firstByte & 0x7f, 1)
        } else {
            // Complex case: multiple bytes
            val valueInThisByte = firstByte & 0x7f
            val (recursiveValue, bytesUsed) = decodeVariableLengthUInt(bytes, startIndex + 1)

            ((recursiveValue << 7) | valueInThisByte, bytesUsed + 1)
        }
    }

    /** Decodes a Byron address (Base58-encoded)
      *
      * @param address
      *   The Base58 address string
      * @return
      *   Success with Byron address type or Failure with exception
      */
    private def decodeByronAddress(address: String): Try[AddressType] = {
        // This implementation is a placeholder - Byron addresses have a complex internal structure
        // that would require a complete implementation of Base58 encoding/decoding and CBOR parsing
        Failure(new UnsupportedOperationException("Byron address decoding not implemented yet"))
    }
}
