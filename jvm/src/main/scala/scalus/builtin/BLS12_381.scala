package scalus.builtin
import scalus.utils.Hex
import supranational.blst.*

import scala.compiletime.asMatchable

class BLS12_381_G1_Element(private[builtin] val value: P1):
    def compressedByteString: ByteString = ByteString.unsafeFromArray(value.compress())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G1_Element => value.is_equal(that.value)
        case _                          => false

    // TODO: check if this is correct
    override def toString: String = s"0x${Hex.bytesToHex(value.compress())}"

class BLS12_381_G2_Element(private[builtin] val value: P2):
    def compressedByteString: ByteString = ByteString.unsafeFromArray(value.compress())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G2_Element => value.is_equal(that.value)
        case _                          => false

    override def toString: String = s"0x${Hex.bytesToHex(value.compress())}"

class BLS12_381_MlResult(private[builtin] val value: PT):
    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_MlResult => value.is_equal(that.value)
        case _                        => false

enum ByteOrder extends Enum[ByteOrder]:
    case BigEndian, LittleEndian
