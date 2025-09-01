package scalus.prelude.offchain

import scalus.builtin.{Data, FromData, ToData}

/**
 * Try to convert Data to T, catching 
 */
def tryFromData[T](d: Data)(using fd: FromData[T]): Either[String, T] =
    try Right(fd(d))
    catch {
        case e: RuntimeException => Left(e.getMessage)
    }
