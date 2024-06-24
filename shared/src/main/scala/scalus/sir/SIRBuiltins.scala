package scalus.sir

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.uplc.DefaultFun

object SIRBuiltins {

    val addInteger: SIR.Builtin = SIR.Builtin(DefaultFun.AddInteger, SIRType.liftM[BigInt=>BigInt=>BigInt])
    val subtractInteger: SIR.Builtin = SIR.Builtin(DefaultFun.SubtractInteger, SIRType.liftM[BigInt => BigInt => BigInt])
    val multiplyInteger = SIR.Builtin(DefaultFun.MultiplyInteger, SIRType.liftM[BigInt=>BigInt=>BigInt])
    val divideInteger  = SIR.Builtin(DefaultFun.DivideInteger, SIRType.liftM[BigInt=>BigInt=>BigInt])
    val quotientInteger = SIR.Builtin(DefaultFun.QuotientInteger, SIRType.liftM[BigInt=>BigInt=>BigInt])
    val remainderInteger = SIR.Builtin(DefaultFun.RemainderInteger, SIRType.liftM[BigInt=>BigInt=>BigInt])
    val modInteger = SIR.Builtin(DefaultFun.ModInteger, SIRType.liftM[BigInt=>BigInt=>BigInt])
    val equalsInteger = SIR.Builtin(DefaultFun.EqualsInteger, SIRType.liftM[BigInt=>BigInt=>Boolean])
    val lessThanInteger = SIR.Builtin(DefaultFun.LessThanInteger, SIRType.liftM[BigInt=>BigInt=>Boolean])
    val lessThanEqualsInteger = SIR.Builtin(DefaultFun.LessThanEqualsInteger, SIRType.liftM[BigInt=>BigInt=>Boolean])

    // Bytestrings
    val appendByteString = SIR.Builtin(DefaultFun.AppendByteString, SIRType.liftM[ByteString=>ByteString=>ByteString])
    val consByteString = SIR.Builtin(DefaultFun.ConsByteString, SIRType.liftM[BigInt=>ByteString=>ByteString])
    val sliceByteString = SIR.Builtin(DefaultFun.SliceByteString, SIRType.liftM[BigInt=>BigInt=>ByteString=>ByteString])
    val lengthOfByteString  = SIR.Builtin(DefaultFun.LengthOfByteString, SIRType.liftM[ByteString=>BigInt])
    val indexByteString  = SIR.Builtin(DefaultFun.IndexByteString, SIRType.liftM[BigInt=>ByteString=>BigInt])
    val equalsByteString = SIR.Builtin(DefaultFun.EqualsByteString, SIRType.liftM[ByteString=>ByteString=>Boolean])
    val lessThanByteString = SIR.Builtin(DefaultFun.LessThanByteString, SIRType.liftM[ByteString=>ByteString=>Boolean])
    val lessThanEqualsByteString = SIR.Builtin(DefaultFun.LessThanEqualsByteString, SIRType.liftM[ByteString=>ByteString=>Boolean])

    // Cryptography and hashes
    val sha2_256 = SIR.Builtin(DefaultFun.Sha2_256, SIRType.liftM[ByteString=>ByteString])
    val sha3_256 = SIR.Builtin(DefaultFun.Sha3_256, SIRType.liftM[ByteString=>ByteString])
    val blake2b_256 = SIR.Builtin(DefaultFun.Blake2b_256, SIRType.liftM[ByteString=>ByteString])
    val verifyEd25519Signature = SIR.Builtin(DefaultFun.VerifyEd25519Signature, SIRType.liftM[ByteString=>ByteString=>ByteString=>Boolean])
    val verifyEcdsaSecp256k1Signature = SIR.Builtin(DefaultFun.VerifyEcdsaSecp256k1Signature, SIRType.liftM[ByteString=>ByteString=>ByteString=>Boolean])
    val verifySchnorrSecp256k1Signature = SIR.Builtin(DefaultFun.VerifySchnorrSecp256k1Signature, SIRType.liftM[(ByteString,ByteString,ByteString)=>Boolean])

    // Strings
    val appendString = SIR.Builtin(DefaultFun.AppendString, SIRType.liftM[(String,String)=>String])
    val equalsString = SIR.Builtin(DefaultFun.EqualsString, SIRType.liftM[(String,String)=>Boolean])
    val encodeUtf8 = SIR.Builtin(DefaultFun.EncodeUtf8, SIRType.liftM[String=>ByteString])
    val decodeUtf8 = SIR.Builtin(DefaultFun.DecodeUtf8, SIRType.liftM[ByteString=>String])

    // Bool
    val ifThenElse = SIR.Builtin(DefaultFun.IfThenElse, SIRType.liftM[[A]=>>((Boolean,A,A)=>A)])

    // Unit
    val chooseUnit = SIR.Builtin(DefaultFun.ChooseUnit, SIRType.liftM[[A]=>>(Unit,A)=>Unit])

    // Tracing
    //   TODO: move to SIR construction
    val trace = SIR.Builtin(DefaultFun.Trace, SIRType.liftM[[A]=>>(String,A)=>Unit])

    // Pairs
    val fstPair = SIR.Builtin(DefaultFun.FstPair, SIRType.liftM[[A,B]=>>(A,B)=>A])
    val sndPair = SIR.Builtin(DefaultFun.SndPair, SIRType.liftM[[A,B]=>>(A,B)=>B])

    // Lists
    val chooseList = SIR.Builtin(DefaultFun.ChooseList, SIRType.liftM[[A,B]=>>(scalus.builtin.List[A],B,B)=>B])
    val mkCons = SIR.Builtin(DefaultFun.MkCons, SIRType.liftM[[A] =>> (A, scalus.builtin.List[A])=> scalus.builtin.List[A]])
    val headList = SIR.Builtin(DefaultFun.HeadList, SIRType.liftM[[A] =>> scalus.builtin.List[A]=>A])
    val tailList = SIR.Builtin(DefaultFun.TailList, SIRType.liftM[[A] =>> scalus.builtin.List[A]=> scalus.builtin.List[A]])
    val nullList = SIR.Builtin(DefaultFun.NullList, SIRType.liftM[[A] =>> scalus.builtin.List[A]=>Boolean])

    // Data
    val chooseData = SIR.Builtin(DefaultFun.ChooseData, SIRType.liftM[[A]=>>(Data,A,A,A,A,A)=>A])
    val constrData = SIR.Builtin(DefaultFun.ConstrData, SIRType.liftM[(BigInt, scalus.builtin.List[Data])=>Data])
    val mapData = SIR.Builtin(DefaultFun.MapData, SIRType.liftM[(Map[BigInt,Data])=>Data])
    val listData = SIR.Builtin(DefaultFun.ListData, SIRType.liftM[scalus.builtin.List[Data]=>Data])
    val iData = SIR.Builtin(DefaultFun.IData, SIRType.liftM[BigInt=>Data])
    val bData = SIR.Builtin(DefaultFun.BData, SIRType.liftM[Boolean=>Data])
    val unConstrData = SIR.Builtin(DefaultFun.UnConstrData, SIRType.liftM[Data=>(BigInt, scalus.builtin.List[Data])])
    val unMapData = SIR.Builtin(DefaultFun.UnMapData, SIRType.liftM[Data=> scalus.builtin.List[(BigInt,Data)]])
    val unListData = SIR.Builtin(DefaultFun.UnListData, SIRType.liftM[Data=>scalus.builtin.List[Data]])
    val unIData = SIR.Builtin(DefaultFun.UnIData, SIRType.liftM[Data=>BigInt])
    val unBData = SIR.Builtin(DefaultFun.UnBData, SIRType.liftM[Data=>Boolean])
    val equalsData = SIR.Builtin(DefaultFun.EqualsData, SIRType.liftM[(Data,Data)=>Boolean])
    val serialiseData = SIR.Builtin(DefaultFun.SerialiseData, SIRType.liftM[Data=>ByteString])

    //   TODO: think about pair
    val mkPairData = SIR.Builtin(DefaultFun.MkPairData, SIRType.liftM[(Data,Data)=>(Data,Data)])
    val mkNilData = SIR.Builtin(DefaultFun.MkNilData, SIRType.liftM[Unit => scalus.builtin.List[Data]])
    val mkNilPairData = SIR.Builtin(DefaultFun.MkNilPairData, SIRType.liftM[Unit => (Data,Data)])


    def fromUplc(uplcFun: DefaultFun): SIR.Builtin = 
        uplcFun match 
            case DefaultFun.AddInteger => addInteger
            case DefaultFun.SubtractInteger => subtractInteger
            case DefaultFun.MultiplyInteger => multiplyInteger
            case DefaultFun.DivideInteger => divideInteger
            case DefaultFun.QuotientInteger => quotientInteger
            case DefaultFun.RemainderInteger => remainderInteger
            case DefaultFun.ModInteger => modInteger
            case DefaultFun.EqualsInteger => equalsInteger
            case DefaultFun.LessThanInteger => lessThanInteger
            case DefaultFun.LessThanEqualsInteger => lessThanEqualsInteger
            case DefaultFun.AppendByteString => appendByteString
            case DefaultFun.ConsByteString => consByteString
            case DefaultFun.SliceByteString => sliceByteString
            case DefaultFun.LengthOfByteString => lengthOfByteString
            case DefaultFun.IndexByteString => indexByteString
            case DefaultFun.EqualsByteString => equalsByteString
            case DefaultFun.LessThanByteString => lessThanByteString
            case DefaultFun.LessThanEqualsByteString => lessThanEqualsByteString
            case DefaultFun.Sha2_256 => sha2_256
            case DefaultFun.Sha3_256 => sha3_256
            case DefaultFun.Blake2b_256 => blake2b_256
            case DefaultFun.VerifyEd25519Signature => verifyEd25519Signature
            case DefaultFun.VerifyEcdsaSecp256k1Signature => verifyEcdsaSecp256k1Signature
            case DefaultFun.VerifySchnorrSecp256k1Signature => verifySchnorrSecp256k1Signature
            case DefaultFun.AppendString => appendString
            case DefaultFun.EqualsString => equalsString
            case DefaultFun.EncodeUtf8 => encodeUtf8
            case DefaultFun.DecodeUtf8 => decodeUtf8
            case DefaultFun.IfThenElse => ifThenElse
            case DefaultFun.ChooseUnit => chooseUnit
            case DefaultFun.Trace => trace
            case DefaultFun.FstPair => fstPair
            case DefaultFun.SndPair => sndPair
            case DefaultFun.ChooseList => chooseList
            case DefaultFun.MkCons => mkCons
            case DefaultFun.HeadList => headList
            case DefaultFun.TailList => tailList
            case DefaultFun.NullList => nullList
            case DefaultFun.ChooseData => chooseData
            case DefaultFun.ConstrData => constrData
            case DefaultFun.MapData => mapData
            case DefaultFun.ListData => listData
            case DefaultFun.IData => iData
            case DefaultFun.BData => bData
            case DefaultFun.UnConstrData => unConstrData
            case DefaultFun.UnMapData => unMapData
            case DefaultFun.UnListData => unListData
            case DefaultFun.UnIData => unIData
            case DefaultFun.UnBData => unBData
            case DefaultFun.EqualsData => equalsData
            case DefaultFun.SerialiseData => serialiseData
            case DefaultFun.MkPairData => mkPairData
            case DefaultFun.MkNilData => mkNilData
            case DefaultFun.MkNilPairData => mkNilPairData
        
    
}
