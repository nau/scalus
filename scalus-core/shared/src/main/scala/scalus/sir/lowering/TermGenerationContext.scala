package scalus.sir.lowering

case class TermGenerationContext(
    generatedVars: Set[String],
    processUndefinedValues: Boolean = false,
    debug: Boolean = false,
) {

    def addGeneratedVar(name: String): TermGenerationContext = {
        copy(generatedVars = generatedVars + name)
    }

}
