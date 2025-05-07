package scalus.builtin

import scalus.CompileDerivations
import scalus.builtin.Builtins.{bData, constrData, encodeUtf8, iData, mkCons, mkNilData}
import scalus.uplc.DefaultFun

import scala.quoted.*

@FunctionalInterface
trait ToData[-A] extends Function1[A, Data] with CompileDerivations {
    override def apply(v1: A): Data
}

/** ToData[A] derivation macros.
  */
@scalus.Compile
object ToData {

    extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]].apply(a)

    inline def derived[A]: ToData[A] = ${
        ToDataMacros.toDataImpl[A]
    }

    @deprecated
    inline def deriveCaseClass[T](inline constrIdx: Int): ToData[T] = ${
        ToDataMacros.deriveCaseClassMacro[T]('{ constrIdx })
    }

    /** Derive a ToData instance for an enum.
      *
      * @tparam T
      *   the enum type
      * @return
      *   a ToData instance for T
      */
    @deprecated
    inline def deriveEnum[T]: ToData[T] = ${ ToDataMacros.deriveEnumMacro[T] }

    given ToData[Boolean] = (a: Boolean) =>
        if a then constrData(1, mkNilData()) else constrData(0, mkNilData())

    @uplcIntrinsic("iData")
    given bigIntToData: ToData[BigInt] = (a: BigInt) => iData(a)
    given ToData[Data] = (a: Data) => a
    @scalus.Ignore
    given ToData[Int] = (a: Int) => iData(a)
    @scalus.Ignore
    given ToData[Long] = (a: Long) => iData(a)
    @uplcIntrinsic("bData")
    given ToData[ByteString] = (a: ByteString) => bData(a)
    given ToData[String] = (a: String) => bData(encodeUtf8(a))
    given ToData[Unit] = (a: Unit) => constrData(0, mkNilData())

    given tupleToData[A: ToData, B: ToData]: ToData[(A, B)] =
        (a: (A, B)) =>
            constrData(
              0,
              mkCons(
                summon[ToData[A]](a._1),
                mkCons(summon[ToData[B]](a._2), mkNilData())
              )
            )

    // TODO: are we need this?
    @scalus.Ignore
    given eitherToData[A: ToData, B: ToData]: ToData[Either[A, B]] =
        (a: Either[A, B]) =>
            a match
                case Left(v)  => constrData(0, mkCons(v.toData, mkNilData()))
                case Right(v) => constrData(1, mkCons(v.toData, mkNilData()))

}
