package scalus

import scalus.macros.Macros
import scalus.sir.SIR
import scalus.uplc.Data

import scala.annotation.Annotation

final class Compile extends Annotation

object Compiler:
  inline def fieldAsData[A](inline expr: A => Any): Data => Data = ${
    Macros.fieldAsDataMacro('expr)
  }
