package scalus.builtins
import io.bullet.borer.Cbor
import scalus.uplc.Data

trait PlatformSpecific:
    def sha2_256(bs: ByteString): ByteString
    def sha3_256(bs: ByteString): ByteString
    def blake2b_256(bs: ByteString): ByteString
    def verifyEd25519Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean
    def verifyEcdsaSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean
    def verifySchnorrSecp256k1Signature(pk: ByteString, msg: ByteString, sig: ByteString): Boolean

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
    def appendByteString(a: ByteString, b: ByteString): ByteString =
        ByteString.fromArray(a.bytes ++ b.bytes)
    def consByteString(char: BigInt, byteString: ByteString): ByteString =
        ByteString.fromArray(char.toByte +: byteString.bytes)
    def sliceByteString(bs: ByteString, start: BigInt, end: BigInt): ByteString =
        if start < 0 || end < 0 || start > end || end > bs.bytes.length then
            throw new Exception(
              s"slice $start $end out of bounds for bytestring of length ${bs.bytes.length}"
            )
        else ByteString.fromArray(bs.bytes.slice(start.toInt, end.toInt))

    def lengthOfByteString(bs: ByteString): BigInt = bs.bytes.length
    def indexByteString(bs: ByteString, i: BigInt): BigInt =
        if i < 0 || i >= bs.bytes.length then
            throw new Exception(
              s"index $i out of bounds for bytestring of length ${bs.bytes.length}"
            )
        else BigInt(bs.bytes(i.toInt) & 0xff)

    def equalsByteString(a: ByteString, b: ByteString): Boolean = a == b
    def lessThanByteString(a: ByteString, b: ByteString): Boolean =
        val minLen = math.min(a.bytes.length, b.bytes.length)
        var i = 0
        while i < minLen do
            val ai = a.bytes(i) & 0xff
            val bi = b.bytes(i) & 0xff
            if ai < bi then return true
            else if ai > bi then return false
            i += 1
        if a.bytes.length < b.bytes.length then true
        else false
    def lessThanEqualsByteString(a: ByteString, b: ByteString): Boolean =
        val minLen = math.min(a.bytes.length, b.bytes.length)
        var i = 0
        while i < minLen do
            val ai = a.bytes(i) & 0xff
            val bi = b.bytes(i) & 0xff
            if ai < bi then return true
            else if ai > bi then return false
            i += 1
        if a.bytes.length <= b.bytes.length then true
        else false
    // Cryptography and hashes
    def sha2_256(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.sha2_256(bs)
    def sha3_256(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.sha3_256(bs)
    def blake2b_256(using ps: PlatformSpecific)(bs: ByteString): ByteString = ps.blake2b_256(bs)
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
    def decodeUtf8(bs: ByteString): String = new String(bs.bytes, "UTF-8")

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

    def mkConstr(ctor: BigInt, args: List[Data]): Data = Data.Constr(ctor.toLong, args.toList)
    def mkMap(values: List[Pair[Data, Data]]): Data =
        Data.Map(values.toList.map(p => (p.fst, p.snd)))
    def mkList(values: List[Data]): Data = Data.List(values.toList)
    def mkI(value: BigInt): Data = Data.I(value)
    def mkB(value: ByteString): Data = Data.B(value)
    def unsafeDataAsConstr(d: Data): Pair[BigInt, List[Data]] = d match
        case Data.Constr(constr, args) => Pair(constr: BigInt, List(args: _*))
        case _                         => throw new Exception(s"not a constructor but $d")
    def unsafeDataAsList(d: Data): List[Data] = d match
        case Data.List(values) => List(values: _*)
        case _                 => throw new Exception(s"not a list but $d")
    def unsafeDataAsMap(d: Data): List[Pair[Data, Data]] = d match
        case Data.Map(values) => List(values.map(Pair.apply): _*)
        case _                => throw new Exception(s"not a list but $d")

    def unsafeDataAsI(d: Data): BigInt = d match
        case Data.I(value) => value
        case _             => throw new Exception(s"not an integer but $d")

    def unsafeDataAsB(d: Data): ByteString = d match
        case Data.B(value) => value
        case _             => throw new Exception(s"not a bytestring but $d")

    def equalsData(d1: Data, d2: Data): Boolean = d1 == d2

    def serialiseData(d: Data): ByteString =
        import scalus.uplc.FlatInstantces.given_Flat_Data.plutusDataCborEncoder
        ByteString.fromArray(Cbor.encode(d).toByteArray)

    // Misc monomorphized constructors.
    // We could simply replace those with constants, but we use built-in functions for consistency
    // with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
    // See note [Representable built-in functions over polymorphic built-in types].
    def mkPairData(fst: Data, snd: Data): Pair[Data, Data] = Pair(fst, snd)
    def mkNilData(): List[Data] = List.empty
    def mkNilPairData(): List[Pair[Data, Data]] = List.empty
