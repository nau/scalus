package scalus.builtin

import scalus.Ignore
import scalus.CompileDerivations
import scalus.builtin.Builtins.decodeUtf8
import scalus.builtin.Builtins.unConstrData
import scalus.builtin.Builtins.unIData
import scalus.builtin.Builtins.unBData
import scalus.builtin.Data.FromData

import scala.quoted.*

@FunctionalInterface
trait FromData[+A] extends Function1[Data, A] with CompileDerivations {

    override def apply(v: Data): A
}

/** FromData[A] derivation
  */
@scalus.Compile
object FromData {

    inline def derived[A]: FromData[A] = ${
        FromDataMacros.fromDataImpl[A]
    }

    @deprecated
    inline def deriveCaseClass[T]: FromData[T] = ${ FromDataMacros.deriveCaseClassMacro[T] }

    /** Derive FromData for an enum type
      *
      * @param conf
      *   a partial function mapping tag to constructor function, like
      * @return
      *   a FromData instance
      *
      * @example
      *   {{{
      *   enum Adt:
      *     case A
      *     case B(b: Boolean)
      *     case C(a: Adt, b: Adt)
      *
      *   given FromData[Adt] = FromData.deriveEnum[Adt] {
      *     case 0 => _ => Adt.A
      *     case 1 => FromData.deriveConstructor[Adt.B]
      *     case 2 => FromData.deriveConstructor[Adt.C]
      *     }
      *   }}}
      */
    @deprecated
    @Ignore
    inline def deriveEnum[T](
        inline conf: PartialFunction[Int, scalus.builtin.List[Data] => T]
    ): FromData[T] = ${ FromDataMacros.deriveEnumMacro[T]('{ conf }) }

    /** Derive FromData for an enum type
      *
      * @return
      *   a FromData instance
      *
      * @example
      *   {{{
      *   enum Adt:
      *     case A
      *     case B(b: Boolean)
      *     case C(a: Adt, b: Adt)
      *
      *   given FromData[Adt] = FromData.deriveEnum[Adt]
      *   }}}
      */
    @deprecated
    @Ignore
    inline def deriveEnum[T]: FromData[T] = ${ FromDataMacros.deriveEnumMacro2[T] }

    @deprecated
    @Ignore
    inline def deriveConstructor[T]: scalus.builtin.List[Data] => T = ${
        FromDataMacros.deriveConstructorMacro[T]
    }

    given FromData[BigInt] = unIData
    given FromData[ByteString] = unBData
    given FromData[String] = (d: Data) => decodeUtf8(unBData(d))
    given FromData[Data] = (d: Data) => d

}
