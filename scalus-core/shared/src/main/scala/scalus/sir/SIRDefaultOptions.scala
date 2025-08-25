package scalus.sir

enum TargetLoweringBackend:
    case SimpleSirToUplcLowering
    case SirToUplc110Lowering
    case SirToUplcV3Lowering

/** Default compiler options for SIR processing. Here to have a single place for default options,
  * which is shared between the compiler plugin and the core library.
  */
object SIRDefaultOptions {

    val targetLoweringBackend: TargetLoweringBackend = TargetLoweringBackend.SimpleSirToUplcLowering
    val generateErrorTraces: Boolean = true
    val optimizeUplc: Boolean = false
    val runtimeLinker: Boolean = true
    val writeSIRToFile: Boolean = false
    val debugLevel: Int = 0

}
