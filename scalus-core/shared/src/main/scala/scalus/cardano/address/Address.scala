package scalus.cardano.address

import scalus.builtin.ByteString
import scalus.cardano.ledger.*

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

import io.bullet.borer.{Decoder, Encoder, Reader, Writer}
import scala.util.control.NonFatal

/** Implementation of Cardano CIP-19 address format following Rust Pallas implementation structure.
  * Handles the binary structure of Cardano addresses including Shelley, Stake, and Byron addresses.
  * Uses strongly typed Hash28 classes for payment, stake and script hashes.
  */

object VarUInt {

    /** Encode positive integer using variable-length encoding as specified in CIP-19 Uses
      * continuation bits to handle arbitrarily large values efficiently
      *
      * @param value
      *   The positive integer to encode (must be >= 0)
      * @return
      *   Encoded bytes with continuation bit protocol
      */
    def encodeVariableLengthUInt(value: Long): Array[Byte] = {
        require(value >= 0, "Value must be non-negative")

        val buffer = mutable.ArrayBuffer.empty[Byte]
        var remaining = value

        while {
            // Take lower 7 bits for this byte
            var currentByte = (remaining & 0x7f).toByte
            remaining >>>= 7

            // Set continuation bit if more bytes follow
            if remaining != 0 then currentByte = (currentByte | 0x80).toByte

            buffer += currentByte
            remaining != 0
        } do ()

        buffer.toArray
    }

    /** Decode variable-length integer as specified in CIP-19 Handles continuation bit protocol to
      * reconstruct original value
      *
      * @param bytes
      *   Source byte array
      * @param startIndex
      *   Index to start decoding from
      * @return
      *   Tuple of (decoded value, number of bytes consumed)
      */
    def decodeVariableLengthUInt(
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
            // Complex case: multiple bytes with continuation
            val valueInThisByte = firstByte & 0x7f
            val (recursiveValue, bytesUsed) = decodeVariableLengthUInt(bytes, startIndex + 1)
            // Shift previous value and combine with current byte
            ((recursiveValue << 7) | valueInThisByte, bytesUsed + 1)
        }
    }
}

import VarUInt.*

/** Network identification for Cardano addresses */
sealed trait Network {

    /** Check if this is mainnet */
    def isMainnet: Boolean = this == Network.Mainnet

    /** Get the numeric value for this network */
    def value: Byte = this match
        case Network.Testnet  => 0x00
        case Network.Mainnet  => 0x01
        case Network.Other(v) => v
}

object Network {
    case object Testnet extends Network {
        override def toString: String = "Testnet"
    }

    case object Mainnet extends Network {
        override def toString: String = "Mainnet"
    }

    case class Other(v: Byte) extends Network {
        require(v >= 2 && v <= 15, s"Invalid network byte: $v, must be in range 2-15")

        override def toString: String = s"Other($v)"
    }

    /** Create Network from byte value */
    def fromByte(value: Byte): Network = value match
        case 0x00 => Testnet
        case 0x01 => Mainnet
        case v    => Other(v)
}

/** Type aliases for clarity - matching Rust implementation */
type TxIdx = Long
type CertIdx = Long

/** Represents a pointer to a stake registration certificate on the blockchain Used in pointer
  * addresses to reference stake credentials indirectly
  *
  * @param slot
  *   Transaction slot number where the stake registration occurred
  * @param txIdx
  *   Transaction index within the slot
  * @param certIdx
  *   Certificate index within the transaction
  */
case class Pointer(slot: Slot, txIdx: TxIdx, certIdx: CertIdx) {

    /** Serialize pointer to variable-length encoded bytes following CIP-19 specification */
    def toBytes: Array[Byte] =
        VarUInt.encodeVariableLengthUInt(slot.slot) ++
            VarUInt.encodeVariableLengthUInt(txIdx) ++
            VarUInt.encodeVariableLengthUInt(certIdx)

    /** Convert to hex string for debugging */
    def toHex: String = toBytes.map("%02x".format(_)).mkString
}

object Pointer {

    /** Parse pointer from bytes starting at given index
      *
      * @param bytes
      *   Source byte array
      * @param startIndex
      *   Index to start parsing from
      * @return
      *   Tuple of (parsed pointer, bytes consumed)
      */
    def parseFrom(bytes: Array[Byte], startIndex: Int): Try[(Pointer, Int)] = Try {
        val (slot, used1) = decodeVariableLengthUInt(bytes, startIndex)
        val (txIdx, used2) = decodeVariableLengthUInt(bytes, startIndex + used1)
        val (certIdx, used3) = decodeVariableLengthUInt(bytes, startIndex + used1 + used2)

        (Pointer(Slot(slot), txIdx, certIdx), used1 + used2 + used3)
    }

    /** Parse pointer from complete byte array */
    def fromBytes(bytes: Array[Byte]): Try[Pointer] =
        parseFrom(bytes, 0).map(_._1)
}

/** The payment part of a Shelley address - can be either a key hash or script hash */
enum ShelleyPaymentPart {
    case Key(hash: AddrKeyHash)
    case Script(hash: ScriptHash)

    /** Get the underlying hash regardless of type */
    def asHash: Hash28 = this match
        case Key(h)    => h
        case Script(h) => h

    /** Check if this represents a script */
    def isScript: Boolean = this match
        case Script(_) => true
        case Key(_)    => false

    /** Convert to bytes */
    def toBytes: ByteString = asHash

    /** Convert to hex string */
    def toHex: String = asHash.toString // Assuming Hash28 has proper toString
}

object ShelleyPaymentPart {

    /** Create from key hash */
    def keyHash(hash: AddrKeyHash): ShelleyPaymentPart = Key(hash)

    /** Create from script hash */
    def scriptHash(hash: ScriptHash): ShelleyPaymentPart = Script(hash)
}

/** The delegation part of a Shelley address - various ways to specify stake credentials */
enum ShelleyDelegationPart {
    case Key(hash: StakeKeyHash)
    case Script(hash: ScriptHash)
    case Pointer(pointer: scalus.cardano.address.Pointer)
    case Null // Enterprise addresses have no delegation part

    /** Get hash if this delegation part contains one */
    def asHash: Option[Hash28] = this match
        case Key(h)                                  => Some(h)
        case Script(h)                               => Some(h)
        case ShelleyDelegationPart.Pointer(_) | Null => None

    /** Check if this represents a script */
    def isScript: Boolean = this match
        case Script(_) => true
        case _         => false

    /** Convert to bytes */
    def toBytes: ByteString = this match
        case Key(h)                           => h
        case Script(h)                        => h
        case ShelleyDelegationPart.Pointer(p) => ByteString.fromArray(p.toBytes)
        case Null                             => ByteString.empty

    /** Convert to hex string */
    def toHex: String = toBytes.toString
}

object ShelleyDelegationPart {

    /** Create from key hash */
    def keyHash(hash: StakeKeyHash): ShelleyDelegationPart = Key(hash)

    /** Create from script hash */
    def scriptHash(hash: ScriptHash): ShelleyDelegationPart = Script(hash)

    /** Create from pointer bytes */
    def fromPointer(bytes: Array[Byte]): Try[ShelleyDelegationPart] =
        scalus.cardano.address.Pointer.fromBytes(bytes).map(Pointer.apply)
}

/** The payload of a Stake address - either stake key or script */
enum StakePayload {
    case Stake(hash: StakeKeyHash)
    case Script(hash: ScriptHash)

    /** Get the underlying hash */
    def asHash: Hash28 = this match
        case Stake(h)  => h
        case Script(h) => h

    /** Check if this represents a script */
    def isScript: Boolean = this match
        case Script(_) => true
        case Stake(_)  => false

    /** Convert to bytes */
    def toBytes: ByteString = asHash

    /** Convert to hex string */
    def toHex: String = asHash.toHex
}

object StakePayload {

    /** Create from stake key hash */
    def stakeKey(hash: StakeKeyHash): StakePayload = Stake(hash)

    /** Create from script hash */
    def script(hash: ScriptHash): StakePayload = Script(hash)

    /** Parse from bytes - assumes 28-byte hash */
    def fromBytes(bytes: Array[Byte], isScript: Boolean): Try[StakePayload] = Try {
        require(bytes.length == 28, s"Invalid hash size: ${bytes.length}, expected 28")
        val hash = ByteString.fromArray(bytes)
        if isScript then Script(Hash.scriptHash(hash)) else Stake(Hash.stakeKeyHash(hash))
    }
}

/** A decoded Shelley address containing network, payment and delegation parts */
case class ShelleyAddress(
    network: Network,
    payment: ShelleyPaymentPart,
    delegation: ShelleyDelegationPart
) {

    /** Get numeric type ID for this address following CIP-19 specification */
    def typeId: Byte = (payment, delegation) match
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Key(_))        => 0x00
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Key(_))     => 0x01
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Script(_))     => 0x02
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Script(_))  => 0x03
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Pointer(_))    => 0x04
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Pointer(_)) => 0x05
        case (ShelleyPaymentPart.Key(_), ShelleyDelegationPart.Null)          => 0x06
        case (ShelleyPaymentPart.Script(_), ShelleyDelegationPart.Null)       => 0x07

    /** Build header byte combining type ID and network */
    def toHeader: Byte = ((typeId << 4) | (network.value & 0x0f)).toByte

    /** Get human-readable prefix for bech32 encoding */
    def hrp: Try[String] = network match
        case Network.Testnet  => Success("addr_test")
        case Network.Mainnet  => Success("addr")
        case Network.Other(x) => Failure(new IllegalArgumentException(s"Unknown network: $x"))

    /** Serialize address to bytes */
    def toBytes: ByteString = {
        val header = ByteString(toHeader)
        val paymentBytes = payment.toBytes
        val delegationBytes = delegation.toBytes
        header ++ paymentBytes ++ delegationBytes
    }

    /** Convert to hex string */
    def toHex: String = toBytes.toString

    /** Encode to bech32 string */
    def toBech32: Try[String] = for {
        prefix <- hrp
        bytes = toBytes.bytes
        encoded <- Try(Bech32.encodeFrom5Bit(prefix, Bech32.to5Bit(bytes)))
    } yield encoded

    /** Check if address contains any script hashes */
    def hasScript: Boolean = payment.isScript || delegation.isScript

    /** Check if this is an enterprise address (no delegation) */
    def isEnterprise: Boolean = delegation == ShelleyDelegationPart.Null
}

/** A decoded Stake address for delegation purposes */
case class StakeAddress(network: Network, payload: StakePayload) {

    /** Get numeric type ID */
    def typeId: Byte = payload match
        case StakePayload.Stake(_)  => 0x0e
        case StakePayload.Script(_) => 0x0f

    /** Build header byte */
    def toHeader: Byte = ((typeId << 4) | (network.value & 0x0f)).toByte

    /** Get human-readable prefix for bech32 encoding */
    def hrp: Try[String] = network match
        case Network.Testnet  => Success("stake_test")
        case Network.Mainnet  => Success("stake")
        case Network.Other(x) => Failure(new IllegalArgumentException(s"Unknown network: $x"))

    /** Serialize to bytes */
    def toBytes: ByteString = ByteString(toHeader) ++ payload.toBytes

    /** Convert to hex string */
    def toHex: String = toBytes.toString

    /** Encode to bech32 string */
    def toBech32: Try[String] = for {
        prefix <- hrp
        bytes = toBytes.bytes
        encoded <- Try(Bech32.encodeFrom5Bit(prefix, Bech32.to5Bit(bytes)))
    } yield encoded

    /** Check if this is a script stake address */
    def isScript: Boolean = payload.isScript
}

/** Placeholder for Byron address - complex legacy format */
case class ByronAddress(bytes: ByteString) {
    def typeId: Byte = 0x08
    def toBytes: ByteString = bytes
    def toHex: String = bytes.toString
    // Byron addresses use Base58 encoding, not implemented here
    def toBase58: String = ??? // Would need Base58 implementation
}

/** Main Address enum representing any type of Cardano address */
enum Address {
    case Byron(address: ByronAddress)
    case Shelley(address: ShelleyAddress)
    case Stake(address: StakeAddress)

    /** Get network if available (Byron addresses don't have explicit network) */
    def network: Option[Network] = this match
        case Byron(_)      => None
        case Shelley(addr) => Some(addr.network)
        case Stake(addr)   => Some(addr.network)

    /** Get type ID */
    def typeId: Byte = this match
        case Byron(addr)   => addr.typeId
        case Shelley(addr) => addr.typeId
        case Stake(addr)   => addr.typeId

    /** Get human-readable prefix if available */
    def hrp: Try[String] = this match
        case Byron(_) =>
            Failure(new UnsupportedOperationException("Byron addresses don't use bech32"))
        case Shelley(addr) => addr.hrp
        case Stake(addr)   => addr.hrp

    /** Check if address contains scripts */
    def hasScript: Boolean = this match
        case Byron(_)      => false
        case Shelley(addr) => addr.hasScript
        case Stake(addr)   => addr.isScript

    /** Check if this is an enterprise address */
    def isEnterprise: Boolean = this match
        case Shelley(addr) => addr.isEnterprise
        case _             => false

    /** Serialize to bytes */
    def toBytes: ByteString = this match
        case Byron(addr)   => addr.toBytes
        case Shelley(addr) => addr.toBytes
        case Stake(addr)   => addr.toBytes

    /** Convert to hex string */
    def toHex: String = this match
        case Byron(addr)   => addr.toHex
        case Shelley(addr) => addr.toHex
        case Stake(addr)   => addr.toHex

    /** Encode to appropriate string format */
    def encode: Try[String] = this match
        case Byron(addr)   => Try(addr.toBase58)
        case Shelley(addr) => addr.toBech32
        case Stake(addr)   => addr.toBech32
}

// Conversion utilities between address types
object Address {

    /** CBOR encoder for Address */
    given Encoder[Address] with
        def write(w: Writer, value: Address): Writer = {
            w.write(value.toBytes)
            w
        }

    /** CBOR decoder for Address */
    given Decoder[Address] with
        def read(r: Reader): Address = {
            val addressBytes = r.read[AddressBytes]()
            try Address.fromByteString(addressBytes)
            catch case NonFatal(exception) => r.validationFailure(exception.getMessage)
        }

    /** Convert Shelley address to Stake address if it has delegation */
    def shelleyToStake(shelleyAddr: ShelleyAddress): Try[StakeAddress] =
        shelleyAddr.delegation match
            case ShelleyDelegationPart.Key(hash) =>
                Success(StakeAddress(shelleyAddr.network, StakePayload.Stake(hash)))
            case ShelleyDelegationPart.Script(hash) =>
                Success(StakeAddress(shelleyAddr.network, StakePayload.Script(hash)))
            case _ =>
                Failure(
                  new IllegalArgumentException(
                    "Cannot convert address without delegation to stake address"
                  )
                )
    // Internal helper functions for variable-length encoding per CIP-19

    // Address parsing and construction functions

    /** Parse address from [[ByteString]]
      *
      * @param bs
      *   Raw address bytes
      * @return
      *   Parsed address
      * @throws IllegalArgumentException
      *   If the byte string is empty or does not match any known address format
      */
    def fromByteString(bs: ByteString): Address = fromBytes(bs.bytes)

    /** Parse address from raw bytes
      *
      * @param bytes
      *   Raw address bytes including header
      * @return
      *   Parsed address or failure with descriptive error
      */
    def fromBytes(bytes: Array[Byte]): Address = {
        require(bytes.nonEmpty, "Address bytes cannot be empty")

        val header = bytes.head
        val payload = bytes.tail

        // Extract type from upper 4 bits of header
        val addressType = (header & 0xf0) >> 4

        addressType match
            case 0x00 => parseType0(header, payload)
            case 0x01 => parseType1(header, payload)
            case 0x02 => parseType2(header, payload)
            case 0x03 => parseType3(header, payload)
            case 0x04 => parseType4(header, payload)
            case 0x05 => parseType5(header, payload)
            case 0x06 => parseType6(header, payload)
            case 0x07 => parseType7(header, payload)
            case 0x08 => parseType8(header, payload)
            case 0x0e => parseType14(header, payload)
            case 0x0f => parseType15(header, payload)
            case _ =>
                throw new IllegalArgumentException(f"Unsupported address type: 0x$addressType%02x")
    }

    /** Parse address from bech32 string */
    def fromBech32(bech32: String): Address = {
        fromBytes(Bech32.decode(bech32).data)
    }

    /** Parse address from any string format (bech32, base58, or hex) */
    def fromString(str: String): Address = {
        // Try bech32 first (most common for modern addresses)
        Try(fromBech32(str))
            .orElse(
              Try(Address.Byron(ByronAddress(ByteString.fromString(str))))
            )
            .get
    }

    // Address type parsers - each handles specific CIP-19 address format

    /** Parse Type 0: Payment Key Hash + Stake Key Hash */
    private def parseType0(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-0 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload.slice(0, 28)))
        val stakeHash = Hash.stakeKeyHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Key(stakeHash)

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 1: Script Hash + Stake Key Hash */
    private def parseType1(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-1 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload.slice(0, 28)))
        val stakeHash = Hash.stakeKeyHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Script(scriptHash)
        val delegation = ShelleyDelegationPart.Key(stakeHash)

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 2: Payment Key Hash + Script Hash */
    private def parseType2(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-2 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload.slice(0, 28)))
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Script(scriptHash)

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 3: Script Hash + Script Hash */
    private def parseType3(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 56,
          s"Invalid Type-3 address length: ${payload.length}, expected 56"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val scriptHash1 = Hash.scriptHash(ByteString.fromArray(payload.slice(0, 28)))
        val scriptHash2 = Hash.scriptHash(ByteString.fromArray(payload.slice(28, 56)))

        val payment = ShelleyPaymentPart.Script(scriptHash1)
        val delegation = ShelleyDelegationPart.Script(scriptHash2)

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 4: Payment Key Hash + Pointer */
    private def parseType4(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length > 28,
          s"Invalid Type-4 address length: ${payload.length}, expected > 28"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload.slice(0, 28)))
        val pointerBytes = payload.slice(28, payload.length)

        val pointer = Pointer
            .fromBytes(pointerBytes)
            .getOrElse(
              throw new IllegalArgumentException("Invalid pointer data")
            )

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Pointer(pointer)

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 5: Script Hash + Pointer */
    private def parseType5(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length > 28,
          s"Invalid Type-5 address length: ${payload.length}, expected > 28"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload.slice(0, 28)))
        val pointerBytes = payload.slice(28, payload.length)

        val pointer = Pointer
            .fromBytes(pointerBytes)
            .getOrElse(
              throw new IllegalArgumentException("Invalid pointer data")
            )

        val payment = ShelleyPaymentPart.Script(scriptHash)
        val delegation = ShelleyDelegationPart.Pointer(pointer)

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 6: Payment Key Hash Only (Enterprise) */
    private def parseType6(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-6 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val paymentHash = AddrKeyHash(ByteString.fromArray(payload))

        val payment = ShelleyPaymentPart.Key(paymentHash)
        val delegation = ShelleyDelegationPart.Null

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 7: Script Hash Only (Enterprise) */
    private def parseType7(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-7 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload))

        val payment = ShelleyPaymentPart.Script(scriptHash)
        val delegation = ShelleyDelegationPart.Null

        Address.Shelley(ShelleyAddress(network, payment, delegation))
    }

    /** Parse Type 8: Byron Address (Legacy) */
    private def parseType8(header: Byte, payload: Array[Byte]): Address = {
        // Byron addresses have complex CBOR structure - simplified here
        val fullBytes = header +: payload
        Address.Byron(ByronAddress(ByteString.fromArray(fullBytes)))
    }

    /** Parse Type 14: Stake Key Hash */
    private def parseType14(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-14 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val stakeHash = Hash.stakeKeyHash(ByteString.fromArray(payload))

        val stakePayload = StakePayload.Stake(stakeHash)
        Address.Stake(StakeAddress(network, stakePayload))
    }

    /** Parse Type 15: Stake Script Hash */
    private def parseType15(header: Byte, payload: Array[Byte]): Address = {
        require(
          payload.length == 28,
          s"Invalid Type-15 address length: ${payload.length}, expected 28"
        )

        val network = Network.fromByte((header & 0x0f).toByte)
        val scriptHash = Hash.scriptHash(ByteString.fromArray(payload))

        val stakePayload = StakePayload.Script(scriptHash)
        Address.Stake(StakeAddress(network, stakePayload))
    }
}
