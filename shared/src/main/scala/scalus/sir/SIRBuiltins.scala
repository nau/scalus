package scalus.sir

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.uplc.DefaultFun

object SIRBuiltins {

    val addInteger: SIR.Builtin = SIR.Builtin(DefaultFun.AddInteger, SIRType.lift[(BigInt,BigInt)=>BigInt])
    val subtractInteger: SIR.Builtin = SIR.Builtin(DefaultFun.SubtractInteger, SIRType.lift[(BigInt, BigInt) => BigInt])
    val multiplyInteger = SIR.Builtin(DefaultFun.MultiplyInteger, SIRType.lift[(BigInt,BigInt)=>BigInt])
    val divideInteger  = SIR.Builtin(DefaultFun.DivideInteger, SIRType.lift[(BigInt,BigInt)=>BigInt])
    val quotientInteger = SIR.Builtin(DefaultFun.QuotientInteger, SIRType.lift[(BigInt,BigInt)=>BigInt])
    val remainderInteger = SIR.Builtin(DefaultFun.RemainderInteger, SIRType.lift[(BigInt,BigInt)=>BigInt])
    val modInteger = SIR.Builtin(DefaultFun.ModInteger, SIRType.lift[(BigInt,BigInt)=>BigInt])
    val equalsInteger = SIR.Builtin(DefaultFun.EqualsInteger, SIRType.lift[(BigInt,BigInt)=>Boolean])
    val lessThanInteger = SIR.Builtin(DefaultFun.LessThanInteger, SIRType.lift[(BigInt,BigInt)=>Boolean])
    val lessThanEqualsInteger = SIR.Builtin(DefaultFun.LessThanEqualsInteger, SIRType.lift[(BigInt,BigInt)=>Boolean])

    // Bytestrings
    val appendByteString = SIR.Builtin(DefaultFun.AppendByteString, SIRType.lift[(ByteString,ByteString)=>ByteString])
    val consByteString = SIR.Builtin(DefaultFun.ConsByteString, SIRType.lift[(BigInt,ByteString)=>ByteString])
    val sliceByteString = SIR.Builtin(DefaultFun.SliceByteString, SIRType.lift[(BigInt,BigInt,ByteString)=>ByteString])
    val lengthOfByteString  = SIR.Builtin(DefaultFun.LengthOfByteString, SIRType.lift[ByteString=>BigInt])
    val indexByteString  = SIR.Builtin(DefaultFun.IndexByteString, SIRType.lift[(BigInt,ByteString)=>BigInt])
    val equalsByteString = SIR.Builtin(DefaultFun.EqualsByteString, SIRType.lift[(ByteString,ByteString)=>Boolean])
    val lessThanByteString = SIR.Builtin(DefaultFun.LessThanByteString, SIRType.lift[(ByteString,ByteString)=>Boolean])
    val lessThanEqualsByteString = SIR.Builtin(DefaultFun.LessThanEqualsByteString, SIRType.lift[(ByteString,ByteString)=>Boolean])

    // Cryptography and hashes
    val sha2_256 = SIR.Builtin(DefaultFun.Sha2_256, SIRType.lift[ByteString=>ByteString])
    val sha3_256 = SIR.Builtin(DefaultFun.Sha3_256, SIRType.lift[ByteString=>ByteString])
    val blake2b_256 = SIR.Builtin(DefaultFun.Blake2b_256, SIRType.lift[ByteString=>ByteString])
    val verifyEd25519Signature = SIR.Builtin(DefaultFun.VerifyEd25519Signature, SIRType.lift[(ByteString,ByteString,ByteString)=>Boolean])
    val verifyEcdsaSecp256k1Signature = SIR.Builtin(DefaultFun.VerifyEcdsaSecp256k1Signature, SIRType.lift[(ByteString,ByteString,ByteString)=>Boolean])
    val verifySchnorrSecp256k1Signature = SIR.Builtin(DefaultFun.VerifySchnorrSecp256k1Signature, SIRType.lift[(ByteString,ByteString,ByteString)=>Boolean])

    // Strings
    val appendString = SIR.Builtin(DefaultFun.AppendString, SIRType.lift[(String,String)=>String])
    val equalsString = SIR.Builtin(DefaultFun.EqualsString, SIRType.lift[(String,String)=>Boolean])
    val encodeUtf8 = SIR.Builtin(DefaultFun.EncodeUtf8, SIRType.lift[String=>ByteString])
    val decodeUtf8 = SIR.Builtin(DefaultFun.DecodeUtf8, SIRType.lift[ByteString=>String])

    // Bool
    val ifThenElse = SIR.Builtin(DefaultFun.IfThenElse, SIRType.liftM[[A]=>>(Boolean,A,A)=>A])

    // Unit
    val chooseUnit = SIR.Builtin(DefaultFun.ChooseUnit, SIRType.liftM[[A]=>>(Unit,A)=>Unit])

    // Tracing
    //   TODO: move to SIR construction
    val trace = SIR.Builtin(DefaultFun.Trace, SIRType.liftM[[A]=>>(String,A)=>Unit])

    // Pairs
    val fstPair = SIR.Builtin(DefaultFun.FstPair, SIRType.liftM[[A,B]=>>(A,B)=>A])
    val sndPair = SIR.Builtin(DefaultFun.SndPair, SIRType.liftM[[A,B]=>>(A,B)=>B])

    // Lists
    val chooseList = SIR.Builtin(DefaultFun.ChooseList, SIRType.liftM[[A,B]=>>(List[A],B,B)=>B])
    val mkCons = SIR.Builtin(DefaultFun.MkCons, SIRType.liftM[[A] =>> (A,List[A])=>List[A]])
    val headList = SIR.Builtin(DefaultFun.HeadList, SIRType.liftM[[A] =>> List[A]=>A])
    val tailList = SIR.Builtin(DefaultFun.TailList, SIRType.liftM[[A] =>> List[A]=>List[A]])
    val nullList = SIR.Builtin(DefaultFun.NullList, SIRType.liftM[[A] =>> List[A]=>Boolean])

    // Data
    val chooseData = SIR.Builtin(DefaultFun.ChooseData, SIRType.liftM[[A]=>>(Data,A,A,A,A,A)=>A])
    val constrData = SIR.Builtin(DefaultFun.ConstrData, SIRType.liftM[(BigInt, List[Data])=>Data])
    val mapData = SIR.Builtin(DefaultFun.MapData, SIRType.liftM[(Map[BigInt,Data])=>Data])
    val listData = SIR.Builtin(DefaultFun.ListData, SIRType.liftM[List[Data]=>Data])
    val iData = SIR.Builtin(DefaultFun.IData, SIRType.liftM[BigInt=>Data])
    val bData = SIR.Builtin(DefaultFun.BData, SIRType.liftM[Boolean=>Data])
    val unConstrData = SIR.Builtin(DefaultFun.UnConstrData, SIRType.liftM[Data=>(BigInt, List[Data])])
    val unMapData = SIR.Builtin(DefaultFun.UnMapData, SIRType.liftM[Data=>List[(BigInt,Data)]])
    val unListData = SIR.Builtin(DefaultFun.UnListData, SIRType.liftM[Data=>List[Data]])
    val unIData = SIR.Builtin(DefaultFun.UnIData, SIRType.lift[Data=>BigInt])
    val unBData = SIR.Builtin(DefaultFun.UnBData, SIRType.lift[Data=>Boolean])
    val equalsData = SIR.Builtin(DefaultFun.EqualsData, SIRType.liftM[(Data,Data)=>Boolean])
    val serialiseData = SIR.Builtin(DefaultFun.SerialiseData, SIRType.lift[Data=>ByteString])

    //   TODO: think about pair
    val mkPairData = SIR.Builtin(DefaultFun.MkPairData, SIRType.liftM[(Data,Data)=>(Data,Data)])
    val mkNilData = SIR.Builtin(DefaultFun.MkNilData, SIRType.liftM[Unit => List[Data]])
    val mkNilPairData = SIR.Builtin(DefaultFun.MkNilPairData, SIRType.liftM[Unit => (Data,Data)])


}
