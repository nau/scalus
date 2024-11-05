package scalus.sir

import scalus.builtin.ByteString
import scalus.builtin.Data
import scalus.uplc.DefaultFun

import scala.util.control.NonFatal

object SIRBuiltins {

    val addInteger: SIR.Builtin = SIR.Builtin(DefaultFun.AddInteger,
                       try
                           SIRTypeMacros.liftM[BigInt=>BigInt=>BigInt]
                       catch
                           case NonFatal(ex) =>
                               println(s"NonFatal Error in addInteger:  ${ex.getMessage}")
                               ex.printStackTrace()
                               throw ex
                           case ex: Throwable =>
                               println(s"Fatal Error in addInteger:  ${ex.getMessage}")
                               ex.printStackTrace()
                               throw ex
                   )
    val subtractInteger: SIR.Builtin = SIR.Builtin(DefaultFun.SubtractInteger, SIRTypeMacros.liftM[BigInt => BigInt => BigInt])
    val multiplyInteger = SIR.Builtin(DefaultFun.MultiplyInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>BigInt])
    val divideInteger  = SIR.Builtin(DefaultFun.DivideInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>BigInt])
    val quotientInteger = SIR.Builtin(DefaultFun.QuotientInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>BigInt])
    val remainderInteger = SIR.Builtin(DefaultFun.RemainderInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>BigInt])
    val modInteger = SIR.Builtin(DefaultFun.ModInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>BigInt])
    val equalsInteger = SIR.Builtin(DefaultFun.EqualsInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>Boolean])
    val lessThanInteger = SIR.Builtin(DefaultFun.LessThanInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>Boolean])
    val lessThanEqualsInteger = SIR.Builtin(DefaultFun.LessThanEqualsInteger, SIRTypeMacros.liftM[BigInt=>BigInt=>Boolean])

    // Bytestrings
    val appendByteString = SIR.Builtin(DefaultFun.AppendByteString, SIRTypeMacros.liftM[ByteString=>ByteString=>ByteString])
    val consByteString = SIR.Builtin(DefaultFun.ConsByteString, SIRTypeMacros.liftM[BigInt=>ByteString=>ByteString])
    val sliceByteString = SIR.Builtin(DefaultFun.SliceByteString, SIRTypeMacros.liftM[BigInt=>BigInt=>ByteString=>ByteString])
    val lengthOfByteString  = SIR.Builtin(DefaultFun.LengthOfByteString, SIRTypeMacros.liftM[ByteString=>BigInt])
    val indexByteString  = SIR.Builtin(DefaultFun.IndexByteString, SIRTypeMacros.liftM[BigInt=>ByteString=>BigInt])
    val equalsByteString = SIR.Builtin(DefaultFun.EqualsByteString, SIRTypeMacros.liftM[ByteString=>ByteString=>Boolean])
    val lessThanByteString = SIR.Builtin(DefaultFun.LessThanByteString, SIRTypeMacros.liftM[ByteString=>ByteString=>Boolean])
    val lessThanEqualsByteString = SIR.Builtin(DefaultFun.LessThanEqualsByteString, SIRTypeMacros.liftM[ByteString=>ByteString=>Boolean])

    // Cryptography and hashes
    val sha2_256 = SIR.Builtin(DefaultFun.Sha2_256, SIRTypeMacros.liftM[ByteString=>ByteString])
    val sha3_256 = SIR.Builtin(DefaultFun.Sha3_256, SIRTypeMacros.liftM[ByteString=>ByteString])
    val blake2b_256 = SIR.Builtin(DefaultFun.Blake2b_256, SIRTypeMacros.liftM[ByteString=>ByteString])
    val verifyEd25519Signature = SIR.Builtin(DefaultFun.VerifyEd25519Signature, SIRTypeMacros.liftM[ByteString=>ByteString=>ByteString=>Boolean])
    val verifyEcdsaSecp256k1Signature = SIR.Builtin(DefaultFun.VerifyEcdsaSecp256k1Signature, SIRTypeMacros.liftM[ByteString=>ByteString=>ByteString=>Boolean])
    val verifySchnorrSecp256k1Signature = SIR.Builtin(DefaultFun.VerifySchnorrSecp256k1Signature, SIRTypeMacros.liftM[(ByteString,ByteString,ByteString)=>Boolean])

    // Strings
    val appendString = SIR.Builtin(DefaultFun.AppendString, SIRTypeMacros.liftM[(String,String)=>String])
    val equalsString = SIR.Builtin(DefaultFun.EqualsString, SIRTypeMacros.liftM[(String,String)=>Boolean])
    val encodeUtf8 = SIR.Builtin(DefaultFun.EncodeUtf8, SIRTypeMacros.liftM[String=>ByteString])
    val decodeUtf8 = SIR.Builtin(DefaultFun.DecodeUtf8, SIRTypeMacros.liftM[ByteString=>String])

    // Bool
    val ifThenElse = SIR.Builtin(DefaultFun.IfThenElse, SIRTypeMacros.liftM[[A]=>>((Boolean,A,A)=>A)])

    // Unit
    val chooseUnit = SIR.Builtin(DefaultFun.ChooseUnit, SIRTypeMacros.liftM[[A]=>>(Unit,A)=>Unit])

    // Tracing
    //   TODO: move to SIR construction
    val trace = SIR.Builtin(DefaultFun.Trace, SIRTypeMacros.liftM[[A]=>>(String,A)=>Unit])

    // Pairs
    val fstPair = SIR.Builtin(DefaultFun.FstPair, SIRTypeMacros.liftM[[A,B]=>>(A,B)=>A])
    val sndPair = SIR.Builtin(DefaultFun.SndPair, SIRTypeMacros.liftM[[A,B]=>>(A,B)=>B])

    // Lists
    val chooseList = SIR.Builtin(DefaultFun.ChooseList, SIRTypeMacros.liftM[[A,B]=>>(scalus.builtin.List[A],B,B)=>B])
    val mkCons = SIR.Builtin(DefaultFun.MkCons, SIRTypeMacros.liftM[[A] =>> (A, scalus.builtin.List[A])=> scalus.builtin.List[A]])
    val headList = SIR.Builtin(DefaultFun.HeadList, SIRTypeMacros.liftM[[A] =>> scalus.builtin.List[A]=>A])
    val tailList = SIR.Builtin(DefaultFun.TailList, SIRTypeMacros.liftM[[A] =>> scalus.builtin.List[A]=> scalus.builtin.List[A]])
    val nullList = SIR.Builtin(DefaultFun.NullList, SIRTypeMacros.liftM[[A] =>> scalus.builtin.List[A]=>Boolean])

    // Data
    val chooseData = SIR.Builtin(DefaultFun.ChooseData, SIRTypeMacros.liftM[[A]=>>(Data,A,A,A,A,A)=>A])
    val constrData = SIR.Builtin(DefaultFun.ConstrData, SIRTypeMacros.liftM[(BigInt, scalus.builtin.List[Data])=>Data])
    val mapData = SIR.Builtin(DefaultFun.MapData, SIRTypeMacros.liftM[(Map[BigInt,Data])=>Data])
    val listData = SIR.Builtin(DefaultFun.ListData, SIRTypeMacros.liftM[scalus.builtin.List[Data]=>Data])
    val iData = SIR.Builtin(DefaultFun.IData, SIRTypeMacros.liftM[BigInt=>Data])
    val bData = SIR.Builtin(DefaultFun.BData, SIRTypeMacros.liftM[Boolean=>Data])
    val unConstrData = SIR.Builtin(DefaultFun.UnConstrData, SIRTypeMacros.liftM[Data=>(BigInt, scalus.builtin.List[Data])])
    val unMapData = SIR.Builtin(DefaultFun.UnMapData, SIRTypeMacros.liftM[Data=> scalus.builtin.List[(BigInt,Data)]])
    val unListData = SIR.Builtin(DefaultFun.UnListData, SIRTypeMacros.liftM[Data=>scalus.builtin.List[Data]])
    val unIData = SIR.Builtin(DefaultFun.UnIData, SIRTypeMacros.liftM[Data=>BigInt])
    val unBData = SIR.Builtin(DefaultFun.UnBData, SIRTypeMacros.liftM[Data=>Boolean])
    val equalsData = SIR.Builtin(DefaultFun.EqualsData, SIRTypeMacros.liftM[(Data,Data)=>Boolean])
    val serialiseData = SIR.Builtin(DefaultFun.SerialiseData, SIRTypeMacros.liftM[Data=>ByteString])

    //   TODO: think about pair
    val mkPairData = SIR.Builtin(DefaultFun.MkPairData, SIRTypeMacros.liftM[(Data,Data)=>(Data,Data)])
    val mkNilData = SIR.Builtin(DefaultFun.MkNilData, SIRTypeMacros.liftM[Unit => scalus.builtin.List[Data]])
    val mkNilPairData = SIR.Builtin(DefaultFun.MkNilPairData, SIRTypeMacros.liftM[Unit => (Data,Data)])


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
