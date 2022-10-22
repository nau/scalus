package scalus.builtins
import scalus.uplc.Data

object Builtins:

  def unsafeDataAsConstr(d: Data): (BigInt, List[Data]) = d match
    case Data.Constr(constr, args) => (constr: BigInt, args)
    case _                         => throw new Exception(s"not a constructor but $d")
  def unsafeDataAsList(d: Data): List[Data] = d match
    case Data.List(values) => values
    case _                 => throw new Exception(s"not a list but $d")

  def unsafeDataAsI(d: Data): BigInt = d match
    case Data.I(value) => value
    case _             => throw new Exception(s"not an integer but $d")

  def unsafeDataAsB(d: Data): Array[Byte] = d match
    case Data.B(value) => value
    case _             => throw new Exception(s"not a bytestring but $d")
