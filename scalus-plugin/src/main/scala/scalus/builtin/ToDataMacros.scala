package scalus.builtin

/** Stubs for compability, to keep Data thr same in compiler and scalus runtime.
  */
object ToDataMacros {
    import scala.quoted.*

    /** This method is a stub for the compiler plugin. The real implementation is in the SIR.
      */
    def toDataImpl[A: Type](using Quotes): Expr[ToData[A]] = {
        ???
    }

    def deriveCaseClassMacro[T: Type](constrIdx: Expr[Int])(using Quotes): Expr[ToData[T]] = {
        ???
    }

    def deriveEnumMacro[T: Type](using Quotes): Expr[ToData[T]] = {
        ???
    }

}
