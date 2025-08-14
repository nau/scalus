package scalus.sir

/** trait which added by compiler plugin to compiled SIR
  */
trait SIRCompiled {

    def sir_ : SIR

    def sirDeps_ : Map[String, SIRCompiled]

}

case class SIRWithDeps(
    sir: SIR,
    sirDeps: Map[String, SIRCompiled]
) extends SIRCompiled {

    override def sir_ : SIR = sir

    override def sirDeps_ : Map[String, SIRCompiled] = sirDeps

}
