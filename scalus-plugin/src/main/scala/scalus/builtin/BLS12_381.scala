package scalus.builtin

class BLS12_381_G1_Element:
    def compressedByteString: ByteString = ???

class BLS12_381_G2_Element:
    def compressedByteString: ByteString = ???

class BLS12_381_MlResult

enum ByteOrder extends Enum[ByteOrder]:
    case BigEndian, LittleEndian
