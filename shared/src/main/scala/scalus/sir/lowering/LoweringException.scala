package scalus.sir.lowering

import scalus.sir.SIRPosition

case class LoweringException(msg: String, pos: SIRPosition, cause: Throwable = null)
    extends RuntimeException(msg, cause) {

}
