package scalus.prelude

import scalus.builtin.{FromData, ToData}

type Data = scalus.builtin.Data
val Data = scalus.builtin.Data

extension [A: ToData](a: A) inline def toData: Data = summon[ToData[A]](a)
extension (inline data: Data) inline def fromData[A](using inline ev: FromData[A]): A = ev(data)
