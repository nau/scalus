package scalus.prelude

import org.scalatest.funsuite.AnyFunSuite

import scalus.prelude.*

object DerivingSpecScope {

    enum AE1 derives ToData:
        case A extends AE1
        case B(b: BigInt) extends AE1
        case C(b: BigInt, bs: ByteString) extends AE1

    // we have old bug: the next is also failed with the same error:
    // import scalus.builtin.ToDataInstances.{*, given}
    // given ae1ToDATA: scalus.builtin.Data.ToData[AE1] = scalus.builtin.ToData.deriveEnum[AE1]

}

class DerivingSpec extends AnyFunSuite {}
