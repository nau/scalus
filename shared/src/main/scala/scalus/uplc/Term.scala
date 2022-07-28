package scalus.uplc

case class Constant(c: String)

sealed trait Term
case class Var(name: String) extends Term
case class LamAbs(name: String, term: Term) extends Term
case class Apply(f: Term, arg: Term) extends Term
case class Force(term: Term) extends Term
case class Delay(term: Term) extends Term
case class Const(const: Constant) extends Term
case class Builtin(bn: DefaultFun) extends Term
case object Error extends Term

case class Program(version: String, term: Term) // FIXME version isn't string

sealed trait DefaultFun {}
// Integers
case object AddInteger extends DefaultFun
case object SubtractInteger extends DefaultFun
case object MultiplyInteger extends DefaultFun
case object DivideInteger extends DefaultFun
case object QuotientInteger extends DefaultFun
case object RemainderInteger extends DefaultFun
case object ModInteger extends DefaultFun
case object EqualsInteger extends DefaultFun
case object LessThanInteger extends DefaultFun
case object LessThanEqualsInteger extends DefaultFun
// Bytestrings
case object AppendByteString extends DefaultFun
case object ConsByteString extends DefaultFun
case object SliceByteString extends DefaultFun
case object LengthOfByteString extends DefaultFun
case object IndexByteString extends DefaultFun
case object EqualsByteString extends DefaultFun
case object LessThanByteString extends DefaultFun
case object LessThanEqualsByteString extends DefaultFun
// Cryptography and hashes
case object Sha2_256 extends DefaultFun
case object Sha3_256 extends DefaultFun
case object Blake2b_256 extends DefaultFun
case object VerifyEd25519Signature extends DefaultFun // formerly verifySignature
case object VerifyEcdsaSecp256k1Signature extends DefaultFun
case object VerifySchnorrSecp256k1Signature extends DefaultFun
// Strings
case object AppendString extends DefaultFun
case object EqualsString extends DefaultFun
case object EncodeUtf8 extends DefaultFun
case object DecodeUtf8 extends DefaultFun
// Bool
case object IfThenElse extends DefaultFun
// Unit
case object ChooseUnit extends DefaultFun
// Tracing
case object Trace extends DefaultFun
// Pairs
case object FstPair extends DefaultFun
case object SndPair extends DefaultFun
// Lists
case object ChooseList extends DefaultFun
case object MkCons extends DefaultFun
case object HeadList extends DefaultFun
case object TailList extends DefaultFun
case object NullList extends DefaultFun
// Data
// See Note [Pattern matching on built-in types].
// It is convenient to have a "choosing" function for a data type that has more than two
// constructors to get pattern matching over it and we may end up having multiple such data
// types, hence we include the name of the data type as a suffix.
case object ChooseData extends DefaultFun
case object ConstrData extends DefaultFun
case object MapData extends DefaultFun
case object ListData extends DefaultFun
case object IData extends DefaultFun
case object BData extends DefaultFun
case object UnConstrData extends DefaultFun
case object UnMapData extends DefaultFun
case object UnListData extends DefaultFun
case object UnIData extends DefaultFun
case object UnBData extends DefaultFun
case object EqualsData extends DefaultFun
case object SerialiseData extends DefaultFun
// Misc monomorphized constructors.
// We could simply replace those with constants, but we use built-in functions for consistency
// with monomorphic built-in types. Polymorphic built-in constructors are generally problematic,
// See note [Representable built-in functions over polymorphic built-in types].
case object MkPairData extends DefaultFun
case object MkNilData extends DefaultFun
case object MkNilPairData extends DefaultFun

sealed trait DefaultUni
case object DefaultUniInteger extends DefaultUni
case object DefaultUniByteString extends DefaultUni
case object DefaultUniString extends DefaultUni
case object DefaultUniUnit extends DefaultUni
case object DefaultUniBool extends DefaultUni
case object DefaultUniProtoList extends DefaultUni
case object DefaultUniProtoPair extends DefaultUni
case object DefaultUniApply extends DefaultUni
case object DefaultUniData extends DefaultUni
