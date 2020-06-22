package Domain

import cats.MonadError
import cats.instances.either._

case class Customer(entryTime: Int, order: Order) {}

object Customer{

  def apply(entryTime: Int, order: Order) = {
    for{
      customer <- validCustomer(entryTime, order)
    } yield customer
  }

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  def validCustomer(entryTime: Int, order: Order) =
    if (entryTime >= 0 && order.cookingTime > 0)
      Right(new Customer(entryTime, order))
    else
      monadError.raiseError("CustomerError")
}
