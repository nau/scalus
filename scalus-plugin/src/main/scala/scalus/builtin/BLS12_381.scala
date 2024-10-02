package scalus.builtin

class BLS12_381_G1_Element:
    def value: ByteString = ???
object BLS12_381_G1_Element:
    def apply(value: ByteString): BLS12_381_G1_Element = ???

class BLS12_381_G2_Element:
    def value: ByteString = ???
object BLS12_381_G2_Element:
    def apply(value: ByteString): BLS12_381_G2_Element = ???

class BLS12_381_MlResult

enum ByteOrder extends Enum[ByteOrder]:
    case BigEndian, LittleEndian
