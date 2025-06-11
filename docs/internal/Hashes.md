# Hash-related types

## Dimensions of choices:

```scala 3
opaque type Hash <: ByteString
opaque type Hash <: Array[Byte]
opaque type Hash <: IArray[Byte]

type Hash[Size <: Int & Singleton]
type Hash[+HF, Size <: Int & Singleton]
type Hash[+HF, Size <: Int & Singleton, +Purpose]
case class Hash28(value: Array[Byte])
case class Hash28(value: IArray[Byte])
case class Hash28(value: ByteString)
```

1. Hash type: `Hash` or `Hash[HF, Size]` or `Hash[HF, Size, Purpose]`
2. opaque vs type alias vs case class
3. over `ByteString` vs `IArray[Byte]` vs `Array[Byte]`

## Compatibility considerations

1. For Java/Kotlin, opaque types are just type aliases, so no type safety there.
2. Wrapping Array[Byte] in a case class is type-safe, but adds overhead.
3. Wrapping ByteString in a case class is type-safe, but adds even more overhead.
4. We use `ByteString` for script evaluation and onchain data types, so it makes sense to use it as the base type for
   `Hash`. This allows us to use the same type for both offchain and onchain data, which is convenient.

So, looks like the compromise is to use `ByteString` as the base type for `Hash`, and use opaque types for type safety.