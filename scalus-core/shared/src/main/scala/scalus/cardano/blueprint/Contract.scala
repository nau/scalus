package scalus.cardano.blueprint

sealed trait Contract {
    def defaultCompiledContract: CompiledContract
    def debugCompiledContract: CompiledContract
    def releaseCompiledContract: CompiledContract
}

object Contract {
    case class PlutusV3Contract private (
        override val defaultCompiledContract: PlutusV3CompiledContract,
        override val debugCompiledContract: PlutusV3CompiledContract,
        override val releaseCompiledContract: PlutusV3CompiledContract
    ) extends Contract

    object PlutusV3Contract {
        inline def apply[D, R](preamble: Preamble, inline code: Any): PlutusV3Contract = {
            val defaultCompiledContract =
                PlutusV3CompiledContract.create[D, R](preamble, scalus.Compiler.Options.default)(
                  code
                )
            val debugCompiledContract =
                PlutusV3CompiledContract.create[D, R](preamble, scalus.Compiler.Options.debug)(code)
            val releaseCompiledContract =
                PlutusV3CompiledContract.create[D, R](preamble, scalus.Compiler.Options.release)(
                  code
                )

            PlutusV3Contract(
              defaultCompiledContract = defaultCompiledContract,
              debugCompiledContract = debugCompiledContract,
              releaseCompiledContract = releaseCompiledContract
            )
        }
    }
}
