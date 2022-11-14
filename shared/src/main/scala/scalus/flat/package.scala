package scalus

import scala.collection.mutable.ListBuffer

package object flat:
  case class Natural(n: BigInt)

  def byteAsBitString(b: Byte): String =
    String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0')

  def encode[A: Flat](a: A, enc: EncoderState): Unit = summon[Flat[A]].encode(a, enc)
  def decode[A: Flat](dec: DecoderState): A = summon[Flat[A]].decode(dec)

  trait Flat[A]:
    def bitSize(a: A): Int
    def encode(a: A, encode: EncoderState): Unit
    def decode(decode: DecoderState): A

  given Flat[Unit] with
    def bitSize(a: Unit): Int = 0
    def encode(a: Unit, encode: EncoderState): Unit = {}
    def decode(decode: DecoderState): Unit = ()

  given Flat[Boolean] with
    def bitSize(a: Boolean): Int = 1
    def encode(a: Boolean, encode: EncoderState): Unit = encode.bits(1, if a then 1 else 0)
    def decode(decode: DecoderState): Boolean =
      decode.bits8(1) match
        case 0 => false
        case 1 => true

  type Uint8Array = Array[Byte]

  /** Prealigned Arrays of bytes PreAligned a ≡ PreAligned {preFiller :: Filler, preValue :: a}
    *
    * Filler ≡ FillerBit Filler \| FillerEnd
    *
    * Array v = A0 \| A1 v (Array v) \| A2 v v (Array v) ... \| A255 ... (Array v)
    */
  class ArrayByteFlat extends Flat[Array[Byte]]:
    def bitSize(a: Array[Byte]): Int = byteArraySize(a)
    def encode(a: Array[Byte], enc: EncoderState): Unit =
      enc.filler() // pre-align
      enc.nextWord()
      var numElems = a.length
      var inx = 0
      var blkLen = Math.min(255, numElems)

      while (blkLen > 0) do
        enc.bits(8, blkLen.toByte)
        var i = 0
        while i < blkLen do
          enc.bits(8, a(inx + i))
          i += 1

        numElems -= blkLen
        inx += blkLen
        blkLen = Math.min(255, numElems)
      enc.bits(8, 0)

    def decode(decode: DecoderState): Array[Byte] =
      decode.filler()
      val size =
        var numElems = decode.buffer(decode.currPtr) & 0xff
        var decoderOffset = numElems + 1
        var size = numElems
        // calculate size
        while numElems == 255 do
          numElems = decode.buffer(decode.currPtr + decoderOffset) & 0xff
          size += numElems
          decoderOffset += numElems + 1
        size

      val result = new Array[Byte](size)
      var numElems = decode.buffer(decode.currPtr) & 0xff
      decode.currPtr += 1
      var resultOffset = 0
      while numElems > 0 do
        Array.copy(decode.buffer, decode.currPtr, result, resultOffset, numElems)
        decode.currPtr += numElems
        resultOffset += numElems
        numElems = decode.buffer(decode.currPtr) & 0xff
        decode.currPtr += 1
      result
  given Flat[Array[Byte]] = ArrayByteFlat()

  given Flat[BigInt] with
    def bitSize(a: BigInt): Int =
      val vs = w7l(zigZag(a))
      vs.length * 8

    // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
    def encode(a: BigInt, encode: EncoderState): Unit =
      val vs = w7l(zigZag(a))
      var i = 0
      while i < vs.length do
        encode.bits(8, vs(i))
        i += 1
    def decode(decode: DecoderState): BigInt =
      var w = decode.bits8(8)
      var r = BigInt(0)
      var shl = 0
      while (w & 0x80) != 0 do
        r = r | (BigInt(w & 0x7f) << shl)
        shl += 7
        w = decode.bits8(8)

      r = r | (BigInt(w & 0x7f) << shl)
      zagZig(r)

  given Flat[Natural] with
    def bitSize(a: Natural): Int =
      val vs = w7l(a.n)
      vs.length * 8

    // Encoded as: data NonEmptyList = Elem Word7 | Cons Word7 NonEmptyList
    def encode(a: Natural, encode: EncoderState): Unit =
      val vs = w7l(a.n)
      var i = 0
      while i < vs.length do
        encode.bits(8, vs(i))
        i += 1

    def decode(decode: DecoderState): Natural =
      var w = decode.bits8(8)
      var r = BigInt(0)
      var shl = 0
      while (w & 0x80) != 0 do
        r = r | (BigInt(w & 0x7f) << shl)
        shl += 7
        w = decode.bits8(8)

      r = r | (BigInt(w & 0x7f) << shl)
      Natural(r)

  given Flat[String] with
    def bitSize(a: String): Int = byteArraySize(a.getBytes("UTF-8"))
    def encode(a: String, encode: EncoderState): Unit =
      summon[Flat[Array[Byte]]].encode(a.getBytes("UTF-8"), encode)

    def decode(decode: DecoderState): String =
      val baDecoder = summon[Flat[Array[Byte]]]
      val bytes = baDecoder.decode(decode)
      new String(bytes, "UTF-8")

  given listFlat[A: Flat]: Flat[List[A]] with
    def bitSize(a: List[A]): Int =
      val flat = summon[Flat[A]]
      a.foldLeft(0)((acc, elem) => acc + flat.bitSize(elem) + 1)

    def encode(a: List[A], encode: EncoderState): Unit =
      val flat = summon[Flat[A]]
      a.foreach { elem =>
        encode.bits(1, 1)
        flat.encode(elem, encode)
      }
      encode.bits(1, 0)

    def decode(decode: DecoderState): List[A] =
      val flat = summon[Flat[A]]
      val result = ListBuffer.empty[A]
      while decode.bits8(1) == 1 do {
        val a = flat.decode(decode)
        result.addOne(a)
      }
      result.toList

  given pairFlat[A: Flat, B: Flat]: Flat[(A, B)] with
    def bitSize(a: (A, B)): Int =
      summon[Flat[A]].bitSize(a._1) + summon[Flat[B]].bitSize(a._2)

    def encode(a: (A, B), encode: EncoderState): Unit =
      summon[Flat[A]].encode(a._1, encode)
      summon[Flat[B]].encode(a._2, encode)

    def decode(decode: DecoderState): (A, B) =
      val a = summon[Flat[A]].decode(decode)
      val b = summon[Flat[B]].decode(decode)
      (a, b)

  def w7l(n: BigInt): List[Byte] =
    val low = n & 0x7f
    val t = n >> 7
    if t == 0 then low.toByte :: Nil else (low | 0x80).toByte :: w7l(t)

  def zigZag(x: BigInt) = if x >= 0 then x << 1 else -(x << 1) - 1
  def zagZig(u: BigInt) = u >> 1 ^ -(u & 1)

  private def arrayBlocks(len: Int): Int =
    val d = len / 255
    d + (if len % 255 == 0 then 0 else 1)

  private def byteArraySize(arr: Uint8Array): Int =
    val numBytes = arr.length + arrayBlocks(arr.length) + 1 + 1 // +1 for pre-align, +1 for end
    8 * numBytes

  class EncoderState(bufferSize: Int):
    val buffer: Array[Byte] = new Array(bufferSize)
    var nextPtr: Int = 0
    var usedBits: Int = 0
    var currentByte: Int = 0

    def result: Array[Byte] =
      val len = if usedBits == 0 then nextPtr else nextPtr + 1
      val result = new Array[Byte](len)
      System.arraycopy(buffer, 0, result, 0, len)
      result

    override def toString: String =
      s"""EncoderState(nextPtr:$nextPtr,usedBits:$usedBits,currentByte:$currentByte,buffer:${buffer
          .map(byteAsBitString)
          .mkString(",")})"""

    // add indicated Int of bits (up to ? bits)
    def bits(numBits: Int, value: Byte): Unit =
      this.usedBits += numBits
      val unusedBits = 8 - this.usedBits
      if unusedBits > 0 then this.currentByte |= value << unusedBits
      else if unusedBits == 0 then
        this.currentByte |= value
        this.nextWord()
      else
        val used = (-unusedBits).toByte
        this.currentByte |= (value & 0xff) >>> used
        this.nextWord()
        this.currentByte = value << (8 - used)
        this.usedBits = used

    def nextWord(): Unit =
      this.buffer(this.nextPtr) = this.currentByte.toByte
      this.nextPtr += 1
      this.currentByte = 0
      this.usedBits = 0

    /** Set last bit of current byte to 1, move to next byte
      */
    def filler(): Unit =
      println(s"currentByte $currentByte usedBits $usedBits")
      this.currentByte |= 1;
      this.buffer(this.nextPtr) = this.currentByte.toByte

  class DecoderState(
      /** The buffer that contains a sequence of flat-encoded values */
      val buffer: Uint8Array
  ):

    /** Pointer to the current byte being decoded (0..buffer.byteLength-1) */
    var currPtr: Int = 0

    /** Number of already decoded bits in the current byte (0..7) */
    var usedBits: Int = 0

    override def toString: String =
      s"""DecoderState(currPtr:$currPtr,usedBits:$usedBits,buffer:${buffer
          .map(byteAsBitString)
          .mkString(",")})"""

    /** Decode up to 8 bits
      * @param numBits
      *   the Int of bits to decode (0..8)
      */
    def bits8(numBits: Int): Byte =
      if numBits < 0 || numBits > 8 then
        throw new RuntimeException("Decoder.bits8: incorrect value of numBits " + numBits)

      this.ensureBits(numBits)
      // usedBits=1 numBits=8 unusedBits=7 leadingZeros=0 unusedBits+leadingZeros=7
      val unusedBits = 8 - this.usedBits
      val leadingZeros = 8 - numBits
      var r =
        ((this.buffer(this.currPtr) << this.usedBits) & 255) >>> leadingZeros

      if numBits > unusedBits then
        val os = byteAsBitString(r.toByte)
        val nextByte: Byte = this.buffer(this.currPtr + 1)
        val nbs = byteAsBitString(nextByte)
        val lowerBits = (nextByte & 255) >>> (unusedBits + leadingZeros)
        val lbs = byteAsBitString(lowerBits.toByte)
        r = r | lowerBits
        val ns = byteAsBitString(r.toByte)
//        println(s"here: was ${os} | $lbs = $ns, nextbyte $nbs, shift: ${unusedBits + leadingZeros}")

      this.dropBits(numBits)

      return (r & 255).toByte

    def filler(): Unit =
      while this.bits8(1) == 0 do ()

    def ensureBits(requiredBits: Int): Unit =
      if requiredBits > this.availableBits() then
        throw new RuntimeException(
          "DecoderState: Not enough data available: " + this.toString
        )

    private def availableBits(): Int =
      return 8 * this.availableBytes() - this.usedBits

    // Available bytes, ignoring used bits
    private def availableBytes(): Int =
      return this.buffer.length - this.currPtr

    private def dropBits(numBits: Int): Unit =
      val totUsed = numBits + this.usedBits
      this.usedBits = totUsed % 8
      this.currPtr += Math.floor(totUsed / 8).toInt
