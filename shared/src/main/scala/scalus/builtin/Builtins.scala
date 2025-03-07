package scalus.builtin
import io.bullet.borer.Cbor

import java.nio.ByteBuffer
import java.nio.CharBuffer
import java.nio.charset.CharsetDecoder
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets
import scalus.uplc.eval.BuiltinException

/** This is the platform specific part of the builtins. This is mostly cryptographic primitives that
  * have different implementations on different platforms.
  */
trait PlatformSpecific:
    def sha2_256(bs: ByteString): ByteString
    def sha3_256(bs: ByteString): ByteString
    def blake2b_224(bs: ByteString): ByteString
    def blake2b_256(bs: ByteString): ByteString
    def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean

    /** Verify an ECDSA signature made using the SECP256k1 curve.
      *
      * @note
      *
      * There are additional well-formation requirements for the arguments beyond their length:
      *
      *   - The first byte of the public key must correspond to the sign of the `y` coordinate: this
      *     is `0x02` if `y` is even, and `0x03` otherwise.
      *   - The remaining bytes of the public key must correspond to the `x` coordinate, as a
      *     big-endian integer.
      *   - The first 32 bytes of the signature must correspond to the big-endian integer
      *     representation of _r_.
      *   - The last 32 bytes of the signature must correspond to the big-endian integer
      *     representation of _s_.
      *
      * While this primitive `accepts` a hash, any caller should only pass it hashes that they
      * computed themselves: specifically, they should receive the `message` from a sender and hash
      * it, rather than receiving the `hash` from said sender. Failure to do so can be
      * [dangerous](https://bitcoin.stackexchange.com/a/81116/35586). Other than length, we make no
      * requirements of what hash gets used.
      * @param pk
      *   Public key (33 bytes)
      * @param msg
      *   Message (32 bytes)
      * @param sig
      *   Signature (64 bytes)
      */
    def verifyEcdsaSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean

    /** Verify a Schnorr signature made using the SECP256k1 curve.
      *
      * @note
      *
      * There are additional well-formation requirements for the arguments beyond their length.
      * Throughout, we refer to co-ordinates of the point `R`.
      *
      *   - The bytes of the public key must correspond to the `x` coordinate, as a big-endian
      *     integer, as specified in BIP-340.
      *   - The first 32 bytes of the signature must correspond to the `x` coordinate, as a
      *     big-endian integer, as specified in BIP-340.
      *   - The last 32 bytes of the signature must correspond to the bytes of `s`, as a big-endian
      *     integer, as specified in BIP-340.
      *
      * @see
      *   [BIP-340](https://github.com/bitcoin/bips/blob/master/bip-0340.mediawiki)
      *
      * @param pk
      *   Public key (32 bytes)
      * @param msg
      *   Message (arbitrary length)
      * @param sig
      *   Signature (64 bytes)
      */
    def verifySchnorrSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean

    // BLS12_381 operations

    def bls12_381_G1_equal(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean

    /** Adds two G1 group elements
      * @param p1
      *   G1 element
      * @param p2
      *   G1 element
      * @return
      *   p1 + p2
      */
    def bls12_381_G1_add(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): BLS12_381_G1_Element

    /** Multiplication of group elements by scalars. In the blst library the arguments are the other
      * way round, but scalars acting on the left is more consistent with standard mathematical
      * practice.
      *
      * @param s
      *   scalar
      * @param p
      *   group element
      * @return
      *   s * p
      */
    def bls12_381_G1_scalarMul(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element

    /** Negates a G1 group element
      *
      * @param p
      *   G1 element
      * @return
      *   -p
      */
    def bls12_381_G1_neg(
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element

    /** Compress a G1 element to a bytestring. This serialises a curve point to its x coordinate
      * only. The compressed bytestring is 48 bytes long, with three spare bits used to convey extra
      * information about the point, including determining which of two possible y coordinates the
      * point has and whether the point is the point at infinity.
      * @see
      *   https://github.com/supranational/blst#serialization-format
      *
      * @param p
      *   G1 element to compress
      * @return
      *   Compressed bytestring
      */
    def bls12_381_G1_compress(p: BLS12_381_G1_Element): ByteString

    /** Uncompress a bytestring to get a G1 point. This will fail if any of the following are true.
      *   - The bytestring is not exactly 48 bytes long.
      *   - The most significant three bits are used incorrectly.
      *   - The bytestring encodes a field element which is not the x coordinate of a point on the
      *     E1 curve.
      *   - The bytestring does represent a point on the E1 curve, but the point is not in the G1
      *     subgroup.
      */
    def bls12_381_G1_uncompress(bs: ByteString): BLS12_381_G1_Element

    def bls12_381_G1_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G1_Element

    def bls12_381_G1_compressed_generator: ByteString

    def bls12_381_G2_equal(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean

    def bls12_381_G2_add(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): BLS12_381_G2_Element

    def bls12_381_G2_scalarMul(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element

    def bls12_381_G2_neg(
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element

    def bls12_381_G2_compress(p: BLS12_381_G2_Element): ByteString

    def bls12_381_G2_uncompress(bs: ByteString): BLS12_381_G2_Element

    def bls12_381_G2_hashToGroup(bs: ByteString, dst: ByteString): BLS12_381_G2_Element

    def bls12_381_G2_compressed_generator: ByteString

    def bls12_381_millerLoop(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult

    def bls12_381_mulMlResult(r1: BLS12_381_MlResult, r2: BLS12_381_MlResult): BLS12_381_MlResult

    def bls12_381_finalVerify(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean

    def keccak_256(bs: ByteString): ByteString

object Builtins:
    // Integers
    def addInteger(i1: BigInt, i2: BigInt): BigInt = i1 + i2
    def subtractInteger(i1: BigInt, i2: BigInt): BigInt = i1 - i2
    def multiplyInteger(i1: BigInt, i2: BigInt): BigInt = i1 * i2
    def divideInteger(i1: BigInt, i2: BigInt): BigInt =
        import java.math.{BigDecimal, RoundingMode}
        val r = new BigDecimal(i1.bigInteger)
            .divide(new BigDecimal(i2.bigInteger), RoundingMode.FLOOR)
            .toBigInteger()
        BigInt(r)
    def quotientInteger(i1: BigInt, i2: BigInt): BigInt = i1 / i2
    def remainderInteger(i1: BigInt, i2: BigInt): BigInt = i1 % i2
    def modInteger(i1: BigInt, i2: BigInt): BigInt =
        /*divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                                     where qr@(q,r) = quotRem n d */
        val r = i1 % i2
        if r.signum == -i2.signum then r + i2 else r
    def equalsInteger(i1: BigInt, i2: BigInt): Boolean = i1 == i2
    def lessThanInteger(i1: BigInt, i2: BigInt): Boolean = i1 < i2
    def lessThanEqualsInteger(i1: BigInt, i2: BigInt): Boolean = i1 <= i2
    // Bytestrings
    def appendByteString(a: ByteString, b: ByteString): ByteString = a ++ b
    def consByteString(char: BigInt, byteString: ByteString): ByteString =
        if char < 0 || char > 255 then
            throw new BuiltinException(s"consByteString: invalid byte value: $char")
        ByteString.fromArray(char.toByte +: byteString.bytes)
    def sliceByteString(start: BigInt, n: BigInt, bs: ByteString): ByteString =
        ByteString.fromArray(bs.bytes.drop(start.toInt).take(n.toInt))

    def lengthOfByteString(bs: ByteString): BigInt = bs.length
    def indexByteString(bs: ByteString, i: BigInt): BigInt =
        if i < 0 || i >= bs.length then
            throw new BuiltinException(
              s"index $i out of bounds for bytestring of length ${bs.length}"
            )
        else BigInt(bs.bytes(i.toInt) & 0xff)

    def equalsByteString(a: ByteString, b: ByteString): Boolean = a == b
    def lessThanByteString(a: ByteString, b: ByteString): Boolean =
        val minLen = math.min(a.length, b.length)
        var i = 0
        while i < minLen do
            val ai = a.bytes(i) & 0xff
            val bi = b.bytes(i) & 0xff
            if ai < bi then return true
            else if ai > bi then return false
            i += 1
        if a.length < b.length then true
        else false
    def lessThanEqualsByteString(a: ByteString, b: ByteString): Boolean =
        val minLen = math.min(a.length, b.length)
        var i = 0
        while i < minLen do
            val ai = a.bytes(i) & 0xff
            val bi = b.bytes(i) & 0xff
            if ai < bi then return true
            else if ai > bi then return false
            i += 1
        if a.length <= b.length then true
        else false
    // Cryptography and hashes
    def sha2_256(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.sha2_256(bs)
    def sha3_256(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.sha3_256(bs)
    def blake2b_256(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.blake2b_256(bs)
    def blake2b_224(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.blake2b_224(bs)
    def verifyEd25519Signature(using ps: PlatformSpecific)(
        pk: ByteString,
        msg: ByteString,
        sig: ByteString
    ): Boolean = ps.verifyEd25519Signature(pk, msg, sig)
    def verifyEcdsaSecp256k1Signature(using
        ps: PlatformSpecific
    )(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ps.verifyEcdsaSecp256k1Signature(pk, msg, sig)
    def verifySchnorrSecp256k1Signature(using
        ps: PlatformSpecific
    )(pk: ByteString, msg: ByteString, sig: ByteString): Boolean =
        ps.verifySchnorrSecp256k1Signature(pk, msg, sig)

    // Strings
    def appendString(s1: String, s2: String): String = s1 + s2
    def equalsString(s1: String, s2: String): Boolean = s1 == s2
    def encodeUtf8(s: String): ByteString = ByteString.fromArray(s.getBytes("UTF-8"))
    def decodeUtf8(bs: ByteString): String =
        UTF8Decoder.decode(bs.bytes)

    // Bool
    def ifThenElse[A](cond: Boolean, a: A, b: A): A =
        if cond then a else b

    // Unit
    def chooseUnit[A]()(a: A): A = a

    // Tracing
    def trace[A](s: String)(a: A): A =
        // calculate the hash
        println(s)
        a

    // Pairs
    def fstPair[A, B](p: Pair[A, B]): A = p.fst
    def sndPair[A, B](p: Pair[A, B]): B = p.snd

    // Lists
    def chooseList[A, B](l: List[A], e: B, ne: B): B =
        if l.isEmpty then e else ne
    def mkCons[A](a: A, l: List[A]): List[A] = a :: l
    def headList[A](l: List[A]): A = l.head
    def tailList[A](l: List[A]): List[A] = l.tail
    def nullList[A](l: List[A]): Boolean = l.isEmpty

    // Data
    def chooseData[A](d: Data, constrCase: A, mapCase: A, listCase: A, iCase: A, bCase: A): A =
        d match
            case Data.Constr(_, _) => constrCase
            case Data.Map(_)       => mapCase
            case Data.List(_)      => listCase
            case Data.I(_)         => iCase
            case Data.B(_)         => bCase

    @deprecated("use constrData", "0.6")
    def mkConstr(ctor: BigInt, args: List[Data]): Data = Data.Constr(ctor.toLong, args.toList)
    def constrData(ctor: BigInt, args: List[Data]): Data = Data.Constr(ctor.toLong, args.toList)
    @deprecated("use mapData", "0.6")
    def mkMap(values: List[Pair[Data, Data]]): Data =
        Data.Map(values.toList.map(p => (p.fst, p.snd)))
    def mapData(values: List[Pair[Data, Data]]): Data =
        Data.Map(values.toList.map(p => (p.fst, p.snd)))
    @deprecated("use listData", "0.6")
    def mkList(values: List[Data]): Data = Data.List(values.toList)
    def listData(values: List[Data]): Data = Data.List(values.toList)
    @deprecated("use iData", "0.6")
    def mkI(value: BigInt): Data = Data.I(value)
    def iData(value: BigInt): Data = Data.I(value)
    @deprecated("use bData", "0.6")
    def mkB(value: ByteString): Data = Data.B(value)
    def bData(value: ByteString): Data = Data.B(value)
    @deprecated("use unConstrData", "0.6")
    def unsafeDataAsConstr(d: Data): Pair[BigInt, List[Data]] = d match
        case Data.Constr(constr, args) => Pair(constr: BigInt, List(args*))
        case _                         => throw new Exception(s"not a constructor but $d")
    def unConstrData(d: Data): Pair[BigInt, List[Data]] = d match
        case Data.Constr(constr, args) => Pair(constr: BigInt, List(args*))
        case _                         => throw new Exception(s"not a constructor but $d")
    @deprecated("use unListData", "0.6")
    def unsafeDataAsList(d: Data): List[Data] = d match
        case Data.List(values) => List(values*)
        case _                 => throw new Exception(s"not a list but $d")
    def unListData(d: Data): List[Data] = d match
        case Data.List(values) => List(values*)
        case _                 => throw new Exception(s"not a list but $d")
    @deprecated("use unMapData", "0.6")
    def unsafeDataAsMap(d: Data): List[Pair[Data, Data]] = d match
        case Data.Map(values) => List(values.map(Pair.apply)*)
        case _                => throw new Exception(s"not a list but $d")

    def unMapData(d: Data): List[Pair[Data, Data]] = d match
        case Data.Map(values) => List(values.map(Pair.apply)*)
        case _                => throw new Exception(s"not a list but $d")
    @deprecated("use unIData", "0.6")
    def unsafeDataAsI(d: Data): BigInt = d match
        case Data.I(value) => value
        case _             => throw new Exception(s"not an integer but $d")
    def unIData(d: Data): BigInt = d match
        case Data.I(value) => value
        case _             => throw new Exception(s"not an integer but $d")
    @deprecated("use unBData", "0.6")
    def unsafeDataAsB(d: Data): ByteString = d match
        case Data.B(value) => value
        case _             => throw new Exception(s"not a bytestring but $d")
    def unBData(d: Data): ByteString = d match
        case Data.B(value) => value
        case _             => throw new Exception(s"not a bytestring but $d")

    def equalsData(d1: Data, d2: Data): Boolean = d1 == d2

    def serialiseData(d: Data): ByteString =
        ByteString.fromArray(Cbor.encode(d).toByteArray)

    // Misc monomorphized constructors.
    // We could simply replace those with constants, but we use built-in functions for consistency
    // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
    // See note [Representable built-in functions over polymorphic built-in types].
    def mkPairData(fst: Data, snd: Data): Pair[Data, Data] = Pair(fst, snd)
    def mkNilData(): List[Data] = List.empty
    def mkNilPairData(): List[Pair[Data, Data]] = List.empty

    /** Conversion from [[BigInt]] to [[ByteString]], as per
      * [CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121).
      */
    def integerToByteString(endianness: Boolean, length: BigInt, input: BigInt): ByteString = {
        IntegerToByteString.integerToByteString(endianness, length, input)
    }

    /** Conversion from [[ByteString]] to [[BigInt]], as per
      * [CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121).
      */
    def byteStringToInteger(endianness: Boolean, input: ByteString): BigInt = {
        ByteStringToInteger.byteStringToInteger(endianness, input)
    }

    /** Bitwise logical AND for ByteStrings.
      * @see
      *   [CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a logical bitwise AND operation on each byte between two ByteStrings sequentially
      * and returns the result(Formally result[i] = lhs[i] & rhs[i]). The argument shouldPad
      * determines what to do in the case when ByteStrings are of different lengths. If shouldPad is
      * false, the result will be the length of the shorter input. Otherwise, the result will be
      * padded with the remaining values from the longer input.
      *
      * @example
      *   andByteString(false, hex"0FFF", hex"FF") == hex"0F"
      * @example
      *   andByteString(true, hex"0FFF", hex"FF") == hex"0FFF"
      *
      * @param shouldPad
      *   Indicates whether to truncate the result to the length of the shorter input, or to pad
      *   with the remaining values from the longer one.
      * @param lhs
      *   The left-hand side `ByteString`.
      * @param rhs
      *   The right-hand side `ByteString`.
      * @return
      *   The result of the bitwise AND operation.
      */
    def andByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        BitwiseLogicalOperations.andByteString(shouldPad, lhs, rhs)

    /** Bitwise logical OR for ByteStrings.
      * @see
      *   [CIP-122](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a logical bitwise OR operation on each byte between two ByteStrings sequentially
      * and returns the result(Formally result[i] = lhs[i] | rhs[i]). The argument shouldPad
      * determines what to do in the case when ByteStrings are of different lengths. If shouldPad is
      * false, the result will be the length of the shorter input. Otherwise, the result will be
      * padded with the remaining values from the longer input.
      *
      * @example
      *   orByteString(false, hex"0FFF", hex"FF") == hex"FF"
      * @example
      *   orByteString(true, hex"0FFF", hex"FF") == hex"FFFF"
      *
      * @param shouldPad
      *   Indicates whether to truncate the result to the length of the shorter input, or to pad
      *   with the remaining values from the longer one.
      * @param lhs
      *   The left-hand side `ByteString`.
      * @param rhs
      *   The right-hand side `ByteString`.
      * @return
      *   The result of the bitwise OR operation.
      */
    def orByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        BitwiseLogicalOperations.orByteString(shouldPad, lhs, rhs)

    /** Bitwise logical XOR for ByteStrings.
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a logical bitwise XOR operation on each byte between two ByteStrings sequentially
      * and returns the result(Formally result[i] = lhs[i] ˆ rhs[i]). The argument shouldPad
      * determines what to do in the case when ByteStrings are of different lengths. If shouldPad is
      * false, the result will be the length of the shorter input. Otherwise, the result will be
      * padded with the remaining values from the longer input.
      *
      * @example
      *   xorByteString(false, hex"0FFF", hex"FF") == hex"F0"
      * @example
      *   xorByteString(true, hex"0FFF", hex"FF") == hex"F0FF"
      *
      * @param shouldPad
      *   Indicates whether to truncate the result to the length of the shorter input, or to pad
      *   with the remaining values from the longer one.
      * @param lhs
      *   The left-hand side `ByteString`.
      * @param rhs
      *   The right-hand side `ByteString`.
      * @return
      *   The result of the bitwise XOR operation.
      */
    def xorByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        BitwiseLogicalOperations.xorByteString(shouldPad, lhs, rhs)

    /** Bitwise logical ComplementByteString for ByteStrings.
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Performs a bitwise logical ComplementByteString operation on the input ByteString by
      * inverting each bit (Formally resultBit[i] = if inputBit[i] == 0 then 1 else 0).
      *
      * @example
      *   complementByteString(hex"FF") == hex"00"
      * @example
      *   complementByteString(hex"F0") == hex"0F"
      *
      * @param byteString
      *   The `ByteString` that to be bitwise logical completed(inverted).
      * @return
      *   The result of the bitwise logical ComplementByteString operation.
      */
    def complementByteString(byteString: ByteString): ByteString =
        BitwiseLogicalOperations.complementByteString(byteString)

    /** Bitwise logical ReadBit for ByteStrings.
      *
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Gets the value of the bit at the specified index in the input ByteString. The index must be
      * in the range [0 .. byteString.size * 8), otherwise BuiltinException will be thrown. Bit
      * indexing starts from the end of the ByteString.
      *
      * @example
      *   readBit(hex"0004", 2) == true
      * @example
      *   readBit(hex"0004", 15) == false
      * @example
      *   readBit(hex"0004", 16) throws BuiltinException
      *
      * @param byteString
      *   The `ByteString` that contains the bit to be read.
      * @param index
      *   The index of the bit to be read.
      * @throws BuiltinException
      *   if the index is out of bounds.
      * @return
      *   The value of the bit at the specified index.
      */
    def readBit(byteString: ByteString, index: BigInt): Boolean =
        BitwiseLogicalOperations.readBit(byteString, index)

    /** Bitwise logical WriteBits for ByteStrings.
      *
      * @see
      *   [CIP-122] (https://github.com/cardano-foundation/CIPs/tree/master/CIP-0122).
      *
      * Sets the value of the bit at the specified indexes in a copy of the input ByteString. The
      * indexes must be in the range [0 .. byteString.size * 8), otherwise BuiltinException will be
      * thrown. Bit indexing starts from the end of the ByteString.
      *
      * @example
      *   writeBits(hex"0000", List(0, 1, 2, 3), true) == hex"000F"
      * @example
      *   writeBits(hex"000F", List(0, 1, 2, 3), false) == hex"0000"
      * @example
      *   writeBits(hex"000F", List(16), true) throws BuiltinException
      *
      * @param byteString
      *   The `ByteString` copy of that to be written.
      * @param indexes
      *   The indexes of the bits to be written.
      * @param bit
      *   The value of the bit to be written.
      * @throws BuiltinException
      *   if the indexes are out of bounds.
      * @return
      *   The result of the bitwise logical WriteBits operation.
      */
    def writeBits(
        byteString: ByteString,
        indexes: scala.collection.immutable.List[BigInt],
        bit: Boolean
    ): ByteString =
        BitwiseLogicalOperations.writeBits(byteString, indexes, bit)

    def bls12_381_G1_equal(using
        ps: PlatformSpecific
    )(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): Boolean =
        ps.bls12_381_G1_equal(p1, p2)

    def bls12_381_G1_add(using
        ps: PlatformSpecific
    )(p1: BLS12_381_G1_Element, p2: BLS12_381_G1_Element): BLS12_381_G1_Element =
        ps.bls12_381_G1_add(p1, p2)

    def bls12_381_G1_scalarMul(using
        ps: PlatformSpecific
    )(s: BigInt, p: BLS12_381_G1_Element): BLS12_381_G1_Element = ps.bls12_381_G1_scalarMul(s, p)

    def bls12_381_G1_neg(using ps: PlatformSpecific)(
        p: BLS12_381_G1_Element
    ): BLS12_381_G1_Element = ps.bls12_381_G1_neg(p)

    def bls12_381_G1_compress(using ps: PlatformSpecific)(p: BLS12_381_G1_Element): ByteString =
        ps.bls12_381_G1_compress(p)

    def bls12_381_G1_uncompress(using ps: PlatformSpecific)(bs: ByteString): BLS12_381_G1_Element =
        ps.bls12_381_G1_uncompress(bs)

    def bls12_381_G1_hashToGroup(using
        ps: PlatformSpecific
    )(bs: ByteString, dst: ByteString): BLS12_381_G1_Element =
        ps.bls12_381_G1_hashToGroup(bs, dst)

    /** The compressed form of the point at infinity in G1, 48 bytes long.
      */
    val bls12_381_G1_compressed_zero: ByteString =
        ByteString.unsafeFromArray(Array(0xc0.toByte) ++ Array.fill(47)(0.toByte))

    def bls12_381_G1_compressed_generator(using ps: PlatformSpecific): ByteString =
        ps.bls12_381_G1_compressed_generator

    def bls12_381_G2_equal(using
        ps: PlatformSpecific
    )(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): Boolean =
        ps.bls12_381_G2_equal(p1, p2)

    def bls12_381_G2_add(using
        ps: PlatformSpecific
    )(p1: BLS12_381_G2_Element, p2: BLS12_381_G2_Element): BLS12_381_G2_Element =
        ps.bls12_381_G2_add(p1, p2)

    def bls12_381_G2_scalarMul(using
        ps: PlatformSpecific
    )(s: BigInt, p: BLS12_381_G2_Element): BLS12_381_G2_Element = ps.bls12_381_G2_scalarMul(s, p)

    def bls12_381_G2_neg(using ps: PlatformSpecific)(
        p: BLS12_381_G2_Element
    ): BLS12_381_G2_Element = ps.bls12_381_G2_neg(p)

    def bls12_381_G2_compress(using ps: PlatformSpecific)(p: BLS12_381_G2_Element): ByteString =
        ps.bls12_381_G2_compress(p)

    def bls12_381_G2_uncompress(using ps: PlatformSpecific)(bs: ByteString): BLS12_381_G2_Element =
        ps.bls12_381_G2_uncompress(bs)

    def bls12_381_G2_hashToGroup(using
        ps: PlatformSpecific
    )(bs: ByteString, dst: ByteString): BLS12_381_G2_Element = ps.bls12_381_G2_hashToGroup(bs, dst)

    /** The compressed form of the point at infinity in G2, 96 bytes long.
      */
    val bls12_381_G2_compressed_zero: ByteString =
        ByteString.unsafeFromArray(Array(0xc0.toByte) ++ Array.fill(95)(0.toByte))

    def bls12_381_G2_compressed_generator(using ps: PlatformSpecific): ByteString =
        ps.bls12_381_G2_compressed_generator

    def bls12_381_millerLoop(using ps: PlatformSpecific)(
        p1: BLS12_381_G1_Element,
        p2: BLS12_381_G2_Element
    ): BLS12_381_MlResult =
        ps.bls12_381_millerLoop(p1, p2)

    def bls12_381_mulMlResult(using
        ps: PlatformSpecific
    )(r1: BLS12_381_MlResult, r2: BLS12_381_MlResult): BLS12_381_MlResult =
        ps.bls12_381_mulMlResult(r1, r2)

    def bls12_381_finalVerify(using
        ps: PlatformSpecific
    )(p1: BLS12_381_MlResult, p2: BLS12_381_MlResult): Boolean =
        ps.bls12_381_finalVerify(p1, p2)

    def keccak_256(using ps: PlatformSpecific)(bs: ByteString): ByteString =
        ps.keccak_256(bs)

private object UTF8Decoder {
    def decode(bytes: Array[Byte]): String = {
        val decoder: CharsetDecoder = StandardCharsets.UTF_8.newDecoder()
        decoder.onMalformedInput(CodingErrorAction.REPORT)
        decoder.onUnmappableCharacter(CodingErrorAction.REPORT)

        val inputBuffer: ByteBuffer = ByteBuffer.wrap(bytes)
        val outputBuffer: CharBuffer = CharBuffer.allocate(bytes.length)

        try
            val result = decoder.decode(inputBuffer, outputBuffer, true)
            if result.isUnderflow then
                outputBuffer.flip()
                outputBuffer.toString
            else throw new IllegalArgumentException("Invalid UTF-8 sequence")
        catch
            case e: java.nio.charset.CharacterCodingException =>
                throw new IllegalArgumentException("Invalid UTF-8 sequence", e)
    }
}
