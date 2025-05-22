package scalus.builtin

import scalus.Ignore
import scalus.CompileDerivations
import scalus.builtin.Builtins.decodeUtf8
import scalus.builtin.Builtins.unConstrData
import scalus.builtin.Builtins.unIData
import scalus.builtin.Builtins.unBData

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

    @uplcIntrinsic("unIData")
    given FromData[BigInt] = unIData
    @uplcIntrinsic("unBData")
    given FromData[ByteString] = unBData
    given FromData[String] = (d: Data) => decodeUtf8(unBData(d))
    given FromData[Data] = (d: Data) => d

    given FromData[Unit] = (d: Data) =>
        if unConstrData(d).fst == BigInt(0) then ()
        else throw new RuntimeException("Not a unit")

    given FromData[Boolean] = (d: Data) =>
        val constr = unConstrData(d).fst
        if constr == BigInt(0) then false
        else if constr == BigInt(1) then true
        else throw new RuntimeException("Not a boolean")

    given unsafeTupleFromData[A, B](using
        fromA: FromData[A],
        fromB: FromData[B]
    ): FromData[(A, B)] =
        (d: Data) =>
            val args = unConstrData(d).snd
            (fromA(args.head), fromB(args.tail.head))

}
