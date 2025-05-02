package scalus.builtin

/** Dummy API for the FromData macros. Real implementation is in the SIR, here just stubs to make
  * compile FromData object shared with SIR
  */
object FromDataMacros {
    import scala.quoted.*

    /** This method is a stub for the compiler plugin. The real implementation is in the SIR.
      */
    def fromDataImpl[A: Type](using Quotes): Expr[FromData[A]] = {
        ???
    }

    def deriveConstructorMacro[T: Type](using
        Quotes
    ): Expr[scalus.builtin.List[Data] => T] = {
        ???
    }

    def deriveCaseClassMacro[T: Type](using Quotes): Expr[FromData[T]] = {
        ???
    }

    def deriveEnumMacro[T: Type](
        conf: Expr[PartialFunction[Int, List[Data] => T]]
    )(using
        Quotes
    ): Expr[FromData[T]] = {
        ???
    }

    def deriveEnumMacro2[T: Type](using Quotes): Expr[FromData[T]] = {
        ???
    }

}
