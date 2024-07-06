package scalus

import dotty.tools.dotc.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Types.*
import dotty.tools.dotc.core.Symbols.*

import scalus.sir.*


object SIRTypesHelper {

    // same as liftM
    def sirType(tp: Type)(using Context): SIRType =
        ???

    def sirTypeInEnv(tp: Type, env: Map[Symbol, SIRType])(using Context): SIRType =
        ???

}
