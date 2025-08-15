package scalus.sir

import scala.annotation.StaticAnnotation

/** trait which added by compiler plugin to compiled SIR
  */
trait SIRCompiled {

    def sirModule_ : Module

    def sirDeps_ : Map[String, Module]

}

case class SIRDependency(name: String, module: Module)

case class SIRWithDeps(
    sirModule: Module,
    sirDeps: Map[String, Module]
) extends SIRCompiled {

    override def sirModule_ : Module = sirModule

    override def sirDeps_ : Map[String, Module] = sirDeps

}

case class SIRBodyAnnotation(module: Module) extends StaticAnnotation
