package scalus

import scalus.macros.Macros
import scalus.uplc.Data
import scalus.sir.SIR

object Compiler:
  inline def fieldAsData[A](inline expr: A => Any): Data => Data = ${
    Macros.fieldAsDataMacro('expr)
  }
