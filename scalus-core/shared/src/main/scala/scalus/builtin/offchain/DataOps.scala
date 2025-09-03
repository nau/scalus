package scalus.builtin.offchain

import scalus.builtin.{Data, FromData, ToData}

import scala.util.control.NonFatal

/** Try to convert Data to T, catching
  */
def tryFromData[T](d: Data)(using fd: FromData[T]): Either[String, T] =
    try Right(fd(d))
    catch {
        case NonFatal(e) =>
            Left(e.getMessage)
    }
