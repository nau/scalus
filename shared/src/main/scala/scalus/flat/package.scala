package scalus

package object flat {
  type Encoder = EncoderState => Unit
  type Decoder = DecoderState => Any

  def byteAsBitString(b: Byte): String =
    String.format("%8s", Integer.toBinaryString(b & 0xff)).replace(' ', '0')

  trait Flat {
    def flatMaxSize(): Int
    def flatEncode: Encoder
  }
  type Uint8Array = Array[Byte]

  private def arrayBlocks(len: Int): Int = {
    Math.ceil(len / 255).toInt + 1
  }

  private def byteArraySize(arr: Uint8Array): Int = {
    val numBytes = arr.length + arrayBlocks(arr.length)
    8 * numBytes
  }

  class EncoderState(bufferSize: Int) {
    val buffer: Array[Byte] = new Array(bufferSize)
    var nextPtr: Int = 0
    var usedBits: Int = 0
    var currentByte: Int = 0

    override def toString: String = {

      s"""EncoderState(nextPtr:$nextPtr,usedBits:$usedBits,currentByte:$currentByte,buffer:${buffer
          .map(byteAsBitString)
          .mkString(",")})"""
    }

    // add indicated Int of bits (up to ? bits)
    def bits(numBits: Int, value: Byte): Unit = {
      this.usedBits += numBits
      val unusedBits = 8 - this.usedBits
      if (unusedBits > 0) this.currentByte |= value << unusedBits
      else if (unusedBits == 0) {
        this.currentByte |= value
        this.nextWord()
      } else {
        val used = (-unusedBits).toByte
        this.currentByte |= (value & 0xff) >>> used
        this.nextWord()
        this.currentByte = value << (8 - used)
        this.usedBits = used
      }
    }

    def nextWord(): Unit = {
      this.buffer(this.nextPtr) = this.currentByte.toByte
      this.nextPtr += 1
      this.currentByte = 0
      this.usedBits = 0
    }

  }

  class DecoderState(
      /** The buffer that contains a sequence of flat-encoded values */
      val buffer: Uint8Array
  ) {

    /** Pointer to the current byte being decoded (0..buffer.byteLength-1) */
    var currPtr: Int = 0

    /** Number of already decoded bits in the current byte (0..7) */
    var usedBits: Int = 0

    override def toString: String = {

      s"""DecoderState(currPtr:$currPtr,usedBits:$usedBits,buffer:${buffer
          .map(byteAsBitString)
          .mkString(",")})"""
    }

    /** Decode up to 8 bits
      * @param numBits
      *   the Int of bits to decode (0..8)
      */
    def bits8(numBits: Int): Byte = {
      if (numBits < 0 || numBits > 8)
        throw new RuntimeException("Decoder.bits8: incorrect value of numBits " + numBits)

      this.ensureBits(numBits)
      // usedBits=1 numBits=8 unusedBits=7 leadingZeros=0 unusedBits+leadingZeros=7
      val unusedBits = 8 - this.usedBits
      val leadingZeros = 8 - numBits
      var r =
        ((this.buffer(this.currPtr) << this.usedBits) & 255) >>> leadingZeros

      if (numBits > unusedBits) {
        val os = byteAsBitString(r.toByte)
        val nextByte: Byte = this.buffer(this.currPtr + 1)
        val nbs = byteAsBitString(nextByte)
        val lowerBits = (nextByte & 255) >>> (unusedBits + leadingZeros)
        val lbs = byteAsBitString(lowerBits.toByte)
        r = r | lowerBits
        val ns = byteAsBitString(r.toByte)
//        println(s"here: was ${os} | $lbs = $ns, nextbyte $nbs, shift: ${unusedBits + leadingZeros}")
      }

      this.dropBits(numBits)

      return (r & 255).toByte
    }

    private def ensureBits(requiredBits: Int): Unit = {
      if (requiredBits > this.availableBits()) {
        throw new RuntimeException(
          "DecoderState: Not enough data available: " + this.toString
        )
      }
    }

    private def availableBits(): Int = {
      return 8 * this.availableBytes() - this.usedBits
    }

    // Available bytes, ignoring used bits
    private def availableBytes(): Int = {
      return this.buffer.length - this.currPtr
    }

    private def dropBits(numBits: Int): Unit = {
      val totUsed = numBits + this.usedBits
      this.usedBits = totUsed % 8
      this.currPtr += Math.floor(totUsed / 8).toInt
    }

  }
}
