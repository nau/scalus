package scalus.sir.lowering

case class TermGenerationContext(
    generatedVars: Set[String]
) {

    def addGeneratedVar(name: String): TermGenerationContext = {
        copy(generatedVars = generatedVars + name)
    }

}
