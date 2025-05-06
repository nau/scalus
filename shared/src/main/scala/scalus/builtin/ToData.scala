package scalus.builtin

import scala.collection.immutable.List
import scala.quoted.*

@FunctionalInterface
trait ToData[-A] extends Function1[A, Data] /*with CompileDerivation*/ {
    override def apply(v1: A): Data
}

/** ToData[A] derivation macros.
  */
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

}
