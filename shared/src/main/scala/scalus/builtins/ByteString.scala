package scalus.builtins

import scalus.utils.Hex

class ByteString private (val bytes: Array[Byte]) {
  override def toString: String = "\"" + toHex + "\""

  override def hashCode(): Int = java.util.Arrays.hashCode(bytes)

  override def equals(obj: Any): Boolean = obj match {
    case that: ByteString => java.util.Arrays.equals(this.bytes, that.bytes)
    case _                => false
  }

  lazy val toHex: String = Hex.bytesToHex(bytes)

}

object ByteString {
  val empty = new ByteString(Array.empty)
  def fromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes.toArray)

  def apply(bytes: Byte*): ByteString = new ByteString(bytes.toArray)

  def unsafeFromArray(bytes: Array[Byte]): ByteString = new ByteString(bytes)
  def fromHex(bytes: String): ByteString = new ByteString(Hex.hexToBytes(bytes))

  implicit class StringInterpolators(val sc: StringContext) extends AnyVal:

    def hex(args: Any*): ByteString =
      val hexString = sc.s(args: _*).replace(" ", "")
      fromHex(hexString)
}
