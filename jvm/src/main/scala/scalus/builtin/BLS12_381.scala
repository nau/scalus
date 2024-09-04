package scalus.builtin
import supranational.blst.*

case class BLS12_381_G1_Element(p: P1):
    def value: ByteString = ???
object BLS12_381_G1_Element:
    def apply(value: ByteString): BLS12_381_G1_Element = BLS12_381_G1_Element(new P1(value.bytes))

case class BLS12_381_G2_Element(p: P2):
    def value: ByteString = ???

object BLS12_381_G2_Element:
    def apply(value: ByteString): BLS12_381_G2_Element = BLS12_381_G2_Element(new P2(value.bytes))

case class BLS12_381_MlResult(value: PT)

enum ByteOrder extends Enum[ByteOrder]:
    case BigEndian, LittleEndian
