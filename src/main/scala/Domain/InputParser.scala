package Domain

import Core.Traverse
import cats.MonadError
import cats.instances.either._

trait InputParser[A, B]{

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  def parse(lines: List[A]): Either[String, B]
}

object CustomersCountParser extends InputParser[String, Int]{

  override def parse(lines: List[String]): Either[String, Int] =
    if (lines.length > 1)  Right(lines.head.toInt)
    else monadError.raiseError("CustomerCountParsingError")

}

object CustomerOrderParser extends InputParser[String, List[List[Int]]] {

  override def parse(lines: List[String]): Either[String, List[List[Int]]] = {

    if(lines.length > 1 && lines.head.toInt == lines.tail.length  ){
      val res = lines.tail.map(line => {
        val customerInfo = line.split("  ").map(_.toInt).toList
        if(customerInfo.size == 2) Right(customerInfo) else monadError.raiseError("CustomerOrderParsingTravError")
      })

      Traverse[String, List[Int]].traverse(res)

//      Traverse2[Either].traverse2(res)
    }
    else monadError.raiseError("CustomerOrderParsingError")

  }

}








