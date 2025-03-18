package scalus.builtin

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Int8Array
import scala.scalajs.js.typedarray.Uint8Array
import NodeJsPlatformSpecific.{toByteString, toJsBigInt, toUint8Array}

import scala.compiletime.asMatchable

class BLS12_381_G1_Element(private val point: BLS.G1.Point):
    def toCompressedByteString: ByteString = point.toRawBytes().toByteString

    def +(that: BLS12_381_G1_Element): BLS12_381_G1_Element = BLS12_381_G1_Element(
      point.add(that.point)
    )

    def *(scalar: BigInt): BLS12_381_G1_Element = BLS12_381_G1_Element(
      point.multiply(scalar.toJsBigInt)
    )

    def unary_- : BLS12_381_G1_Element = BLS12_381_G1_Element(point.negate())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G1_Element => point.isEquals(that.point)
        case _                          => false

    override def hashCode: Int = toCompressedByteString.hashCode
    override def toString: String = s"0x${point.toHex()}"

object BLS12_381_G1_Element:
    def fromCompressedByteString(byteString: ByteString): BLS12_381_G1_Element =
        BLS12_381_G1_Element(
          BLS.G1.Point.fromRowBytes(byteString.toUint8Array)
        )

    def hashToGroup(byteString: ByteString, dst: ByteString): BLS12_381_G1_Element =
        BLS12_381_G1_Element(
          BLS.g1
              .hashToGroup(byteString.toUint8Array, BLS.HtfBasicOpts(dst.toUint8Array))
        )

class BLS12_381_G2_Element(private val point: BLS.G2.Point):
    def toCompressedByteString: ByteString = point.toRawBytes().toByteString

    def +(that: BLS12_381_G2_Element): BLS12_381_G2_Element = BLS12_381_G2_Element(
      point.add(that.point)
    )

    def *(scalar: BigInt): BLS12_381_G2_Element = BLS12_381_G2_Element(
      point.multiply(scalar.toJsBigInt)
    )

    def unary_- : BLS12_381_G2_Element = BLS12_381_G2_Element(point.negate())

    override def equals(that: Any): Boolean = that.asMatchable match
        case that: BLS12_381_G2_Element => point.isEquals(that.point)
        case _                          => false

    override def hashCode: Int = toCompressedByteString.hashCode
    override def toString: String = s"0x${point.toHex()}"

object BLS12_381_G2_Element:
    def fromCompressedByteString(byteString: ByteString): BLS12_381_G2_Element =
        BLS12_381_G2_Element(
          BLS.G2.Point.fromRowBytes(byteString.toUint8Array)
        )

    def hashToGroup(byteString: ByteString, dst: ByteString): BLS12_381_G2_Element =
        BLS12_381_G2_Element(
          BLS.g2
              .hashToGroup(byteString.toUint8Array, BLS.HtfBasicOpts(dst.toUint8Array))
        )

class BLS12_381_MlResult(value: ByteString)

enum ByteOrder extends Enum[ByteOrder]:
    case BigEndian, LittleEndian

private[builtin] object BLS:
    // import { bls12_381 as bls } from '@noble/curves/bls12-381
    @JSImport("@noble/curves/bls12-381", "bls12_381")
    @js.native
    private object bls12_381 extends js.Object:
        def G1: G1 = js.native
        def G2: G2 = js.native

    def g1: G1 = bls12_381.G1
    def g2: G2 = bls12_381.G2

    class HtfBasicOpts(val DST: Uint8Array) extends js.Object

    @js.native
    trait G1 extends js.Object:
        @JSName("ProjectivePoint")
        def pointModule: G1.PointModule = js.native

        @JSName("hashToCurve")
        def hashToGroup(msg: Uint8Array, options: HtfBasicOpts): G1.Point = js.native

    object G1:
        @js.native
        trait PointModule extends js.Object:
            @JSName("fromHex")
            def fromRowBytes(bytes: Uint8Array): Point = js.native

        @js.native
        trait Point extends js.Object:
            @JSName("equals")
            def isEquals(other: Point): Boolean = js.native
            def add(other: Point): Point = js.native
            def multiply(scalar: js.BigInt): Point = js.native
            def negate(): Point = js.native
            def toRawBytes(isCompressed: Boolean = true): Uint8Array = js.native
            def toHex(isCompressed: Boolean = true): String = js.native

        object Point:
            def fromRowBytes(bytes: Uint8Array): Point = g1.pointModule.fromRowBytes(bytes)
    end G1

    @js.native
    trait G2 extends js.Object:
        @JSName("ProjectivePoint")
        def pointModule: G2.PointModule = js.native

        @JSName("hashToCurve")
        def hashToGroup(msg: Uint8Array, options: HtfBasicOpts): G2.Point = js.native

    object G2:
        @js.native
        trait PointModule extends js.Object:
            @JSName("fromHex")
            def fromRowBytes(bytes: Uint8Array): Point = js.native

        @js.native
        trait Point extends js.Object:
            @JSName("equals")
            def isEquals(other: Point): Boolean = js.native
            def add(other: Point): Point = js.native
            def multiply(scalar: js.BigInt): Point = js.native
            def negate(): Point = js.native
            def toRawBytes(isCompressed: Boolean = true): Uint8Array = js.native
            def toHex(isCompressed: Boolean = true): String = js.native

        object Point:
            def fromRowBytes(bytes: Uint8Array): Point = g2.pointModule.fromRowBytes(bytes)
    end G2
end BLS
