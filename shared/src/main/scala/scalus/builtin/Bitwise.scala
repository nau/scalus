package scalus.builtin

import scalus.uplc.eval.BuiltinException

import scala.collection.mutable.ArrayBuffer

object IntegerToByteString:
    val maximumOutputLength: Int = 8192

    private def integerLog2(i: BigInt): Int =
        if i <= 0 then 0
        else i.bitLength - 1

    private enum IntegerToByteStringError:
        case NegativeInput, NotEnoughDigits

    /** Conversion from [[BigInt]] to [[ByteString]], as per
      * [CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121).
      */
    def integerToByteString(
        endiannessArg: Boolean,
        lengthArg: BigInt,
        input: BigInt
    ): ByteString =
        if lengthArg < 0 then
            throw new BuiltinException(
              s"integerToByteString: negative length argument\nLength requested: $lengthArg"
            )
        else if lengthArg > maximumOutputLength then
            throw new BuiltinException(
              s"integerToByteString: requested length is too long (maximum is $maximumOutputLength bytes)\nLength requested: $lengthArg"
            )
        else if lengthArg == 0 && integerLog2(input) >= 8 * maximumOutputLength then
            val bytesRequiredFor = (n: BigInt) => integerLog2(n) / 8 + 1
            throw new BuiltinException(
              s"integerToByteString: input too long (maximum is 2^${8 * maximumOutputLength}-1)\nLength required: ${bytesRequiredFor(input)}"
            )
        else
            val endianness = if endiannessArg then ByteOrder.BigEndian else ByteOrder.LittleEndian
            unsafeIntegerToByteString(endianness, lengthArg.toInt, input) match
                case Left(IntegerToByteStringError.NegativeInput) =>
                    throw new BuiltinException(
                      s"integerToByteString: cannot convert negative Integer\nInput: $input"
                    )
                case Left(IntegerToByteStringError.NotEnoughDigits) =>
                    throw new BuiltinException(
                      s"integerToByteString: cannot represent Integer in given number of bytes\nInput: $input\nBytes requested: $lengthArg"
                    )
                case Right(result) => result

    private def unsafeIntegerToByteString(
        requestedByteOrder: ByteOrder,
        requestedLength: Int,
        input: BigInt
    ): Either[IntegerToByteStringError, ByteString] =
        if input < 0 then Left(IntegerToByteStringError.NegativeInput)
        else if input == 0 then Right(ByteString.unsafeFromArray(new Array[Byte](requestedLength)))
        else if requestedLength == 0 then
            val result = requestedByteOrder match
                case ByteOrder.LittleEndian => goLENoLimit(input)
                case ByteOrder.BigEndian    => goLENoLimit(input).reverse
            Right(ByteString.unsafeFromArray(result.toArray))
        else
            val result = requestedByteOrder match
                case ByteOrder.LittleEndian => goLELimit(input, requestedLength)
                case ByteOrder.BigEndian    => goLELimit(input, requestedLength).map(_.reverse)
            result match
                case None        => Left(IntegerToByteStringError.NotEnoughDigits)
                case Some(bytes) => Right(ByteString.unsafeFromArray(bytes.toArray))

    private def goLELimit(remaining: BigInt, requestedLength: Int): Option[ArrayBuffer[Byte]] =
        val builder = new ArrayBuffer[Byte](requestedLength)
        var current = remaining

        while current != 0 && builder.length < requestedLength do
            val byte = (current & 0xff).toByte
            builder += byte
            current = current >> 8

        if current == 0 then
            val result = builder.padTo(requestedLength, 0.toByte)
            Some(result)
        else None

    private def goLENoLimit(input: BigInt): ArrayBuffer[Byte] =
        val builder = new ArrayBuffer[Byte](input.bitLength / 8 + 1)
        var remaining = input

        var current = remaining

        while current != 0 do
            val byte = (current & 0xff).toByte
            builder += byte
            current = current >> 8

        builder

object ByteStringToInteger:
    enum ByteOrder:
        case LittleEndian, BigEndian

    /** Conversion from [[ByteString]] to [[BigInt]], as per
      * [CIP-121](https://github.com/cardano-foundation/CIPs/tree/master/CIP-0121).
      */
    def byteStringToInteger(statedEndiannessArg: Boolean, input: ByteString): BigInt =
        val endianness = if statedEndiannessArg then ByteOrder.BigEndian else ByteOrder.LittleEndian
        unsafeByteStringToInteger(endianness, input)

    private def unsafeByteStringToInteger(statedByteOrder: ByteOrder, input: ByteString): BigInt =
        val bytes = statedByteOrder match
            case ByteOrder.LittleEndian =>
                input.bytes.view.reverse
            case ByteOrder.BigEndian =>
                input.bytes.view
        val nonZeroInput = bytes.dropWhile(_ == 0)
        if nonZeroInput.isEmpty then BigInt(0)
        else goBE(nonZeroInput.toArray)

    private def goBE(input: Array[Byte]): BigInt =
        var result = BigInt(0)
        var i = 0
        while i < input.size do
            val byte = input(i).toInt & 0xff
            result = (result << 8) + BigInt(byte)
            i += 1
        result

    private def reverseTakeWhile(bs: ByteString, p: Byte => Boolean): Array[Byte] =
        var lastNonZeroIndex = bs.size - 1
        while lastNonZeroIndex >= 0 && p(bs.bytes(lastNonZeroIndex)) do lastNonZeroIndex -= 1
        if lastNonZeroIndex == -1 then Array.empty
        else bs.bytes.slice(0, lastNonZeroIndex + 1)

object BitwiseLogicalOperations:
    def andByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        combineByteStrings(shouldPad, lhs, rhs)((lhsByte, rhsByte) => (lhsByte & rhsByte).toByte)

    def orByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        combineByteStrings(shouldPad, lhs, rhs)((lhsByte, rhsByte) => (lhsByte | rhsByte).toByte)

    def xorByteString(shouldPad: Boolean, lhs: ByteString, rhs: ByteString): ByteString =
        combineByteStrings(shouldPad, lhs, rhs)((lhsByte, rhsByte) => (lhsByte ^ rhsByte).toByte)

    def complementByteString(byteString: ByteString): ByteString =
        transformByteString(byteString)(byte => (byte ^ 255).toByte)

    def readBit(byteString: ByteString, index: BigInt): Boolean = {
        if byteString.isEmpty then
            throw new BuiltinException(
              s"readBit: Index out of bounds, because byte string is empty, actual: $index"
            )

        val bytes = byteString.bytes
        val bitLength = bytes.length * 8

        if index < 0 || index >= bitLength then
            throw new BuiltinException(
              s"readBit: Index out of bounds, expected: [0 .. $bitLength), actual: $index"
            )

        val currentIndex = index.toInt
        val byteIndex = (bitLength - 1 - currentIndex) / 8
        val bitIndex = currentIndex % 8
        ((bytes(byteIndex) >> bitIndex) & 1) == 1
    }

    def writeBits(
        byteString: ByteString,
        indexes: Seq[BigInt],
        bit: Boolean
    ): ByteString = {
        if indexes.isEmpty then return byteString

        if byteString.isEmpty then
            throw new BuiltinException(
              s"writeBits: Indexes out of bounds, because byte string is empty, actual: $indexes"
            )

        val resultArray = byteString.bytes.clone()
        val bitLength = resultArray.length * 8
        val bitValue = if bit then 1 else 0

        for index <- indexes do
            if index < 0 || index >= bitLength then
                throw new BuiltinException(
                  s"writeBits: Index out of bounds, expected: [0 .. $bitLength), actual: $index"
                )

            val currentIndex = index.toInt
            val byteIndex = (bitLength - 1 - currentIndex) / 8
            val bitIndex = currentIndex % 8
            if bitValue == 1 then
                resultArray(byteIndex) = (resultArray(byteIndex) | (1 << bitIndex)).toByte
            else resultArray(byteIndex) = (resultArray(byteIndex) & ~(1 << bitIndex)).toByte

        ByteString.unsafeFromArray(resultArray)
    }

    def replicateByte(length: BigInt, byte: BigInt): ByteString = {
        if length < 0 || length > IntegerToByteString.maximumOutputLength then
            throw new BuiltinException(
              s"replicateByte: requested length out of bounds, expected: [0 .. ${IntegerToByteString.maximumOutputLength}], actual: $length"
            )

        if byte < 0 || byte > 255 then
            throw new BuiltinException(
              s"replicateByte: byte value out of bounds, expected: [0 .. 255], actual: $byte"
            )

        val lengthValue = length.toInt
        val byteValue = byte.toByte

        if lengthValue == 0 then return ByteString.empty

        val resultArray = new Array[Byte](lengthValue)
        if byteValue != 0 then java.util.Arrays.fill(resultArray, byteValue)
        ByteString.unsafeFromArray(resultArray)
    }

    def shiftByteString(byteString: ByteString, shift: BigInt): ByteString = {
        if byteString.isEmpty || shift == 0 then return byteString

        val bytes = byteString.bytes
        val bytesLength = bytes.length

        if bytesLength * 8 < shift.abs then
            return ByteString.unsafeFromArray(new Array[Byte](bytesLength))

        if shift < Int.MinValue || shift > Int.MaxValue then
            throw new BuiltinException(
              s"shiftByteString: shift out of bounds, expected: [${Int.MinValue} .. ${Int.MaxValue}], actual: $shift"
            )

        val shiftValue = shift.toInt

        val resultArray =
            if shiftValue > 0 then shiftLeft(bytes, shiftValue)
            else shiftRight(bytes, shiftValue.abs)

        ByteString.unsafeFromArray(resultArray)
    }

    def rotateByteString(byteString: ByteString, rotation: BigInt): ByteString = {
        val bytes = byteString.bytes
        val bytesLength = bytes.length
        val bitLength = bytesLength * 8
        val rotationRemainder = rotation % bitLength

        if bytesLength == 0 || rotationRemainder == 0 then return byteString

        if rotationRemainder < Int.MinValue || rotationRemainder > Int.MaxValue then
            throw new BuiltinException(
              s"rotateByteString: rotation remainder to big, expected: [${Int.MinValue} .. ${Int.MaxValue}], actual: $rotationRemainder"
            )

        val rotationValue = rotationRemainder.toInt
        ???
//        val resultArray =
        //            if shiftValue > 0 then shiftLeft(bytes, shiftValue)
        //            else shiftRight(bytes, shiftValue.abs)
        //
        //        ByteString.unsafeFromArray(resultArray)
    }

    private inline def combineByteStrings(
        shouldPad: Boolean,
        lhs: ByteString,
        rhs: ByteString
    )(inline op: (Byte, Byte) => Byte): ByteString = {
        val (shortArray, longArray) =
            if lhs.size < rhs.size then (lhs.bytes, rhs.bytes) else (rhs.bytes, lhs.bytes)

        val resultArray = new Array[Byte](if shouldPad then longArray.length else shortArray.length)

        var index = 0
        val shortArrayLength = shortArray.length
        while index < shortArrayLength do
            resultArray(index) = op(shortArray(index), longArray(index))
            index += 1

        if shouldPad && shortArray.length != longArray.length then
            System.arraycopy(
              longArray,
              shortArray.length,
              resultArray,
              shortArray.length,
              longArray.length - shortArray.length
            )

        ByteString.unsafeFromArray(resultArray)
    }

    private inline def transformByteString(
        byteString: ByteString
    )(inline op: Byte => Byte): ByteString = {
        val inputArray = byteString.bytes
        val inputArrayLength = inputArray.length
        val resultArray = new Array[Byte](inputArrayLength)

        var index = 0
        while index < inputArrayLength do
            resultArray(index) = op(inputArray(index))
            index += 1

        ByteString.unsafeFromArray(resultArray)
    }

    private def shiftLeft(inputBytes: Array[Byte], shift: Int): Array[Byte] = {
        val shiftMod = shift % 8
        val carryMask = ((1 << shiftMod) - 1).toByte
        val offsetBytes = shift / 8
        val bytesLength = inputBytes.length

        val resultBytes = new Array[Byte](bytesLength)

        var index = 0
        while index < bytesLength do
            val sourceIndex = index + offsetBytes

            if sourceIndex >= bytesLength then resultBytes(index) = 0
            else
                val src = inputBytes(sourceIndex)
                var dst = (src << shiftMod).toByte
                if sourceIndex + 1 < bytesLength then
                    dst = (dst | ((inputBytes(
                      sourceIndex + 1
                    ) >>> (8 - shiftMod)) & carryMask)).toByte
                resultBytes(index) = dst

            index += 1

        resultBytes
    }

    private def shiftRight(inputBytes: Array[Byte], shift: Int): Array[Byte] = {
        val shiftMod = shift % 8
        val carryMask = (0xff << (8 - shiftMod)).toByte
        val offsetBytes = shift / 8
        val bytesLength = inputBytes.length

        val resultBytes = new Array[Byte](bytesLength)

        var index = bytesLength - 1
        while index >= 0 do
            val sourceIndex = index - offsetBytes

            if sourceIndex < 0 then resultBytes(index) = 0
            else
                val src = inputBytes(sourceIndex)
                var dst = ((src & 0xff) >>> shiftMod).toByte
                if sourceIndex - 1 >= 0 then
                    dst =
                        (dst | ((inputBytes(sourceIndex - 1) << (8 - shiftMod)) & carryMask)).toByte
                resultBytes(index) = dst

            index -= 1

        resultBytes
    }
