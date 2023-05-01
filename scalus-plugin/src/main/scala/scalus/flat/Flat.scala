package scalus.flat

import scalus.builtins
import scalus.sir.Binding
import scalus.sir.Case
import scalus.sir.ConstrDecl
import scalus.sir.DataDecl
import scalus.sir.Recursivity
import scalus.sir.SIR
import scalus.uplc.Constant
import scalus.uplc.DefaultFun
import scalus.uplc.DefaultUni
import scalus.uplc.NamedDeBruijn

import scala.collection.mutable.ListBuffer

object Flat:
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

    def filler(): Unit =
      this.currentByte |= 1;
      nextWord()

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

object FlatInstantces:
  import Flat.{*, given}
  val constantWidth = 4
  val termTagWidth = 4

  given Flat[builtins.ByteString] with
    val flatArray = summon[Flat[Array[Byte]]]
    def bitSize(a: builtins.ByteString): Int = flatArray.bitSize(a.bytes)

    def encode(a: builtins.ByteString, encode: EncoderState): Unit =
      flatArray.encode(a.bytes, encode)

    def decode(decode: DecoderState): builtins.ByteString =
      builtins.ByteString.unsafeFromArray(flatArray.decode(decode))

  def flatForUni(uni: DefaultUni): Flat[Any] =
    import DefaultUni.*
    uni match
      case Integer    => summon[Flat[BigInt]].asInstanceOf[Flat[Any]]
      case ByteString => summon[Flat[builtins.ByteString]].asInstanceOf[Flat[Any]]
      case String     => summon[Flat[String]].asInstanceOf[Flat[Any]]
      case Unit       => summon[Flat[Unit]].asInstanceOf[Flat[Any]]
      case Bool       => summon[Flat[Boolean]].asInstanceOf[Flat[Any]]
      // FIXME
      // case Data                => summon[Flat[Data]].asInstanceOf[Flat[Any]]
      case Apply(ProtoList, a) => listFlat(flatForUni(a)).asInstanceOf[Flat[Any]]
      case Apply(Apply(ProtoPair, a), b) =>
        pairFlat(flatForUni(a), flatForUni(b)).asInstanceOf[Flat[Any]]

  def encodeUni(uni: DefaultUni): List[Int] =
    uni match
      case DefaultUni.Integer           => List(0)
      case DefaultUni.ByteString        => List(1)
      case DefaultUni.String            => List(2)
      case DefaultUni.Unit              => List(3)
      case DefaultUni.Bool              => List(4)
      case DefaultUni.ProtoList         => List(5)
      case DefaultUni.ProtoPair         => List(6)
      case DefaultUni.Apply(uniF, uniA) => 7 :: encodeUni(uniF) ++ encodeUni(uniA)
      case DefaultUni.Data              => List(8)

  def decodeUni(state: List[Int]): (DefaultUni, List[Int]) =
    state match
      case 0 :: tail => (DefaultUni.Integer, tail)
      case 1 :: tail => (DefaultUni.ByteString, tail)
      case 2 :: tail => (DefaultUni.String, tail)
      case 3 :: tail => (DefaultUni.Unit, tail)
      case 4 :: tail => (DefaultUni.Bool, tail)
      case 5 :: tail => (DefaultUni.ProtoList, tail)
      case 6 :: tail => (DefaultUni.ProtoPair, tail)
      case 7 :: tail =>
        val (uniF, tail1) = decodeUni(tail)
        val (uniA, tail2) = decodeUni(tail1)
        (DefaultUni.Apply(uniF, uniA), tail2)
      case 8 :: tail => (DefaultUni.Data, tail)

  given Flat[DefaultFun] with
    import DefaultFun.*

    def bitSize(a: DefaultFun): Int = 7

    def encode(a: DefaultFun, encode: EncoderState): Unit =
      val code = a match
        case AddInteger            => 0
        case SubtractInteger       => 1
        case MultiplyInteger       => 2
        case DivideInteger         => 3
        case QuotientInteger       => 4
        case RemainderInteger      => 5
        case ModInteger            => 6
        case EqualsInteger         => 7
        case LessThanInteger       => 8
        case LessThanEqualsInteger => 9

        case AppendByteString         => 10
        case ConsByteString           => 11
        case SliceByteString          => 12
        case LengthOfByteString       => 13
        case IndexByteString          => 14
        case EqualsByteString         => 15
        case LessThanByteString       => 16
        case LessThanEqualsByteString => 17

        case Sha2_256                        => 18
        case Sha3_256                        => 19
        case Blake2b_256                     => 20
        case VerifyEd25519Signature          => 21
        case VerifyEcdsaSecp256k1Signature   => 52
        case VerifySchnorrSecp256k1Signature => 53

        case AppendString => 22
        case EqualsString => 23
        case EncodeUtf8   => 24
        case DecodeUtf8   => 25

        case IfThenElse => 26

        case ChooseUnit => 27

        case Trace => 28

        case FstPair => 29
        case SndPair => 30

        case ChooseList => 31
        case MkCons     => 32
        case HeadList   => 33
        case TailList   => 34
        case NullList   => 35

        case ChooseData    => 36
        case ConstrData    => 37
        case MapData       => 38
        case ListData      => 39
        case IData         => 40
        case BData         => 41
        case UnConstrData  => 42
        case UnMapData     => 43
        case UnListData    => 44
        case UnIData       => 45
        case UnBData       => 46
        case EqualsData    => 47
        case MkPairData    => 48
        case MkNilData     => 49
        case MkNilPairData => 50
        case SerialiseData => 51
      encode.bits(7, code.toByte)

    def decode(decode: DecoderState): DefaultFun =
      decode.bits8(7) match
        case 0  => AddInteger
        case 1  => SubtractInteger
        case 2  => MultiplyInteger
        case 3  => DivideInteger
        case 4  => QuotientInteger
        case 5  => RemainderInteger
        case 6  => ModInteger
        case 7  => EqualsInteger
        case 8  => LessThanInteger
        case 9  => LessThanEqualsInteger
        case 10 => AppendByteString
        case 11 => ConsByteString
        case 12 => SliceByteString
        case 13 => LengthOfByteString
        case 14 => IndexByteString
        case 15 => EqualsByteString
        case 16 => LessThanByteString
        case 17 => LessThanEqualsByteString
        case 18 => Sha2_256
        case 19 => Sha3_256
        case 20 => Blake2b_256
        case 21 => VerifyEd25519Signature
        case 22 => AppendString
        case 23 => EqualsString
        case 24 => EncodeUtf8
        case 25 => DecodeUtf8
        case 26 => IfThenElse
        case 27 => ChooseUnit
        case 28 => Trace
        case 29 => FstPair
        case 30 => SndPair
        case 31 => ChooseList
        case 32 => MkCons
        case 33 => HeadList
        case 34 => TailList
        case 35 => NullList
        case 36 => ChooseData
        case 37 => ConstrData
        case 38 => MapData
        case 39 => ListData
        case 40 => IData
        case 41 => BData
        case 42 => UnConstrData
        case 43 => UnMapData
        case 44 => UnListData
        case 45 => UnIData
        case 46 => UnBData
        case 47 => EqualsData
        case 48 => MkPairData
        case 49 => MkNilData
        case 50 => MkNilPairData
        case 51 => SerialiseData
        case 52 => VerifyEcdsaSecp256k1Signature
        case 53 => VerifySchnorrSecp256k1Signature
        case c  => throw new Exception(s"Invalid builtin function code: $c")

  /* given Flat[Data] with
    implicit val plutusDataCborEncoder: Encoder[Data] = PlutusDataCborEncoder
    implicit val plutusDataCborDecoder: Decoder[Data] = PlutusDataCborDecoder

    def bitSize(a: Data): Int = summon[Flat[Array[Byte]]].bitSize(Cbor.encode(a).toByteArray)

    def encode(a: Data, encode: EncoderState): Unit =
      flat.encode(Cbor.encode(a).toByteArray, encode)

    def decode(decode: DecoderState): Data =
      val bytes = summon[Flat[Array[Byte]]].decode(decode)
      Cbor.decode(bytes).to[Data].value */

  given Flat[Constant] with

    val constantTypeTagFlat = new Flat[Int]:
      def bitSize(a: Int): Int = constantWidth

      def encode(a: Int, encode: EncoderState): Unit = encode.bits(constantWidth, a.toByte)

      def decode(decode: DecoderState): Int = decode.bits8(constantWidth)

    def bitSize(a: Constant): Int =
      val uniSize = encodeUni(
        a.tpe
      ).length * (1 + constantWidth) + 1 // List Cons (1 bit) + constant + List Nil (1 bit)
      val valueSize = flatForUni(a.tpe).bitSize(Constant.toValue(a))
      uniSize + valueSize

    def encode(a: Constant, encoder: EncoderState): Unit =
      val tags = encodeUni(a.tpe)
      listFlat[Int](constantTypeTagFlat).encode(tags, encoder)
      flatForUni(a.tpe).encode(Constant.toValue(a), encoder)

    def decode(decoder: DecoderState): Constant =
      import DefaultUni.*
      val tags = listFlat[Int](constantTypeTagFlat).decode(decoder)
      val (tpe, _) = decodeUni(tags)
      val uniDecoder = flatForUni(tpe)
      val decoded = uniDecoder.decode(decoder)
      val result = Constant.fromValue(tpe, decoded)
      result

  given Flat[Recursivity] with
    def bitSize(a: Recursivity): Int = 1

    def encode(a: Recursivity, encode: EncoderState): Unit =
      encode.bits(1, if a == Recursivity.Rec then 1 else 0)

    def decode(decode: DecoderState): Recursivity =
      if decode.bits8(1) == 1 then Recursivity.Rec else Recursivity.NonRec

  given Flat[Binding] with
    def bitSize(a: Binding): Int =
      val nameSize = summon[Flat[String]].bitSize(a.name)
      val termSize = summon[Flat[SIR]].bitSize(a.value)
      nameSize + termSize
    def encode(a: Binding, encode: EncoderState): Unit =
      summon[Flat[String]].encode(a.name, encode)
      summon[Flat[SIR]].encode(a.value, encode)
    def decode(decode: DecoderState): Binding =
      val name = summon[Flat[String]].decode(decode)
      val term = summon[Flat[SIR]].decode(decode)
      Binding(name, term)

  given Flat[ConstrDecl] with
    def bitSize(a: ConstrDecl): Int =
      val nameSize = summon[Flat[String]].bitSize(a.name)
      val paramsSize = summon[Flat[List[String]]].bitSize(a.params)
      nameSize + paramsSize
    def encode(a: ConstrDecl, encode: EncoderState): Unit = {
      summon[Flat[String]].encode(a.name, encode)
      summon[Flat[List[String]]].encode(a.params, encode)
    }
    def decode(decode: DecoderState): ConstrDecl = {
      val name = summon[Flat[String]].decode(decode)
      val params = summon[Flat[List[String]]].decode(decode)
      ConstrDecl(name, params)
    }

  given Flat[DataDecl] with
    def bitSize(a: DataDecl): Int =
      val nameSize = summon[Flat[String]].bitSize(a.name)
      val constrSize = listFlat[ConstrDecl].bitSize(a.constructors)
      nameSize + constrSize
    def encode(a: DataDecl, encode: EncoderState): Unit =
      summon[Flat[String]].encode(a.name, encode)
      listFlat[ConstrDecl].encode(a.constructors, encode)
    def decode(decode: DecoderState): DataDecl =
      val name = summon[Flat[String]].decode(decode)
      val constr = listFlat[ConstrDecl].decode(decode)
      DataDecl(name, constr)

  given Flat[Case] with {
    def bitSize(a: Case): Int =
      val constrSize = summon[Flat[ConstrDecl]].bitSize(a.constr)
      val bindings = summon[Flat[List[String]]].bitSize(a.bindings)
      val bodySize = summon[Flat[SIR]].bitSize(a.body)
      constrSize + bindings + bodySize
    def encode(a: Case, encode: EncoderState): Unit = {
      summon[Flat[ConstrDecl]].encode(a.constr, encode)
      summon[Flat[List[String]]].encode(a.bindings, encode)
      summon[Flat[SIR]].encode(a.body, encode)
    }
    def decode(decode: DecoderState): Case = {
      val constr = summon[Flat[ConstrDecl]].decode(decode)
      val bindings = summon[Flat[List[String]]].decode(decode)
      val body = summon[Flat[SIR]].decode(decode)
      Case(constr, bindings, body)
    }
  }

  given Flat[SIR] with
    import SIR.*

    def bitSize(a: SIR): Int = a match
      case Var(name) =>
        // in Plutus See Note [Index (Word64) (de)serialized through Natural]
        termTagWidth + summon[Flat[String]].bitSize(name.name)
      case Let(rec, binds, body) =>
        termTagWidth + summon[Flat[Recursivity]].bitSize(rec) + listFlat[Binding].bitSize(
          binds
        ) + bitSize(body)
      case LamAbs(x, t)        => termTagWidth + bitSize(t)
      case Apply(f, x)         => termTagWidth + bitSize(f) + bitSize(x)
      case Const(c)            => termTagWidth + summon[Flat[Constant]].bitSize(c)
      case IfThenElse(c, t, f) => termTagWidth + bitSize(c) + bitSize(t) + bitSize(f)
      case Builtin(bn)         => termTagWidth + summon[Flat[DefaultFun]].bitSize(bn)
      case Error(msg)          => termTagWidth + summon[Flat[String]].bitSize(msg)
      case Decl(data, term) => termTagWidth + summon[Flat[DataDecl]].bitSize(data) + bitSize(term)
      case Constr(name, data, args) =>
        termTagWidth + summon[Flat[String]].bitSize(name) + summon[Flat[DataDecl]].bitSize(
          data
        ) + listFlat[SIR].bitSize(args)
      case Match(scrutinee, cases) =>
        termTagWidth + bitSize(scrutinee) + listFlat[Case].bitSize(cases)

    def encode(a: SIR, enc: EncoderState): Unit =
      a match
        case Var(name) =>
          enc.bits(termTagWidth, 0)
          summon[Flat[String]].encode(name.name, enc)
        case Let(rec, binds, body) =>
          enc.bits(termTagWidth, 1)
          summon[Flat[Recursivity]].encode(rec, enc)
          listFlat[Binding].encode(binds, enc)
          encode(body, enc)
        case LamAbs(x, t) =>
          enc.bits(termTagWidth, 2)
          encode(t, enc)
        case Apply(f, x) =>
          enc.bits(termTagWidth, 3)
          encode(f, enc)
          encode(x, enc)
        case Const(c) =>
          enc.bits(termTagWidth, 4)
          summon[Flat[Constant]].encode(c, enc)
        case IfThenElse(c, t, f) =>
          enc.bits(termTagWidth, 5)
          encode(c, enc)
          encode(t, enc)
          encode(f, enc)
        case Builtin(bn) =>
          enc.bits(termTagWidth, 6)
          summon[Flat[DefaultFun]].encode(bn, enc)
        case Error(msg) =>
          enc.bits(termTagWidth, 7)
          summon[Flat[String]].encode(msg, enc)
        case Decl(data, term) =>
          enc.bits(termTagWidth, 8)
          summon[Flat[DataDecl]].encode(data, enc)
          encode(term, enc)
        case Constr(name, data, args) =>
          enc.bits(termTagWidth, 9)
          summon[Flat[String]].encode(name, enc)
          summon[Flat[DataDecl]].encode(data, enc)
          listFlat[SIR].encode(args, enc)
        case Match(scrutinee, cases) =>
          enc.bits(termTagWidth, 10)
          encode(scrutinee, enc)
          listFlat[Case].encode(cases, enc)

    def decode(decoder: DecoderState): SIR =
      val tag = decoder.bits8(termTagWidth)
      tag match
        case 0 =>
          val name = summon[Flat[String]].decode(decoder)
          Var(NamedDeBruijn(name))
        case 1 =>
          val rec = summon[Flat[Recursivity]].decode(decoder)
          val binds = listFlat[Binding].decode(decoder)
          val body = decode(decoder)
          Let(rec, binds, body)
        case 2 =>
          val t = decode(decoder)
          LamAbs("x", t)
        case 3 =>
          val f = decode(decoder)
          val x = decode(decoder)
          Apply(f, x)
        case 4 =>
          val c = summon[Flat[Constant]].decode(decoder)
          Const(c)
        case 5 =>
          val c = decode(decoder)
          val t = decode(decoder)
          val f = decode(decoder)
          IfThenElse(c, t, f)
        case 6 =>
          val bn = summon[Flat[DefaultFun]].decode(decoder)
          Builtin(bn)
        case 7 =>
          val msg = summon[Flat[String]].decode(decoder)
          Error(msg)
        case 8 =>
          val data = summon[Flat[DataDecl]].decode(decoder)
          val term = decode(decoder)
          Decl(data, term)
        case 9 =>
          val name = summon[Flat[String]].decode(decoder)
          val data = summon[Flat[DataDecl]].decode(decoder)
          val args = listFlat[SIR].decode(decoder)
          Constr(name, data, args)
        case 10 =>
          val scrutinee = decode(decoder)
          val cases = listFlat[Case].decode(decoder)
          Match(scrutinee, cases)
