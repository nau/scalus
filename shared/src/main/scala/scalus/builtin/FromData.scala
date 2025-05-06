package scalus.builtin

import scala.quoted.*

@FunctionalInterface
trait FromData[+A] extends Function1[Data, A] /*with CompileDerivations*/ {
    // compiler derivation is unaviable inside compiler.  Now disable, later - maybe copy.

    override def apply(v: Data): A
}

/** FromData[A] derivation
  */
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
    inline def deriveEnum[T]: FromData[T] = ${ FromDataMacros.deriveEnumMacro2[T] }

    @deprecated
    inline def deriveConstructor[T]: scalus.builtin.List[Data] => T = ${
        FromDataMacros.deriveConstructorMacro[T]
    }

}
