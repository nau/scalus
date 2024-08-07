package scalus.builtin

case class BLS12_381_G1_Element(value: ByteString)
case class BLS12_381_G2_Element(value: ByteString)
case class BLS12_381_MlResult(value: ByteString)

enum ByteOrder extends Enum[ByteOrder]:
    case BigEndian, LittleEndian
