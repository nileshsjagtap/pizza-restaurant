package Domain

import cats.{Monad, MonadError}
import cats.instances.either._
import cats.implicits._

import scala.io.Source
import scala.io.Source.fromFile
import scala.util.{Failure, Success, Try}

trait Reader{
  def read(filePath: String): Either[String, List[String]]
}

object InputReader extends Reader {


  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  def read1(filePath: String): Either[String, List[String]] = {
    Try(fromFile(filePath).getLines.toList) match {
      case Success(lines) => Right(lines)
      case Failure(_) => monadError.raiseError("InputParsingError")
    }
  }

  def read[F[_]: MonadError](filePath: String): F[List[String]] = {
    val M = implicitly[MonadError[F, String]]

    Source.fromFile(filePath)
    if (filePath.isEmpty) M.raiseError("FilePath is empty")
    else M.pure(List("Some", "Data"))
  }

//  def readFrom[F[_]](filePath: String)(implicit M: MonadError[F, ?]): F[List[String]] = {
//    if (filePath.isEmpty) "FilePath is empty"
//    else List("Some", "Data")
//  }

}