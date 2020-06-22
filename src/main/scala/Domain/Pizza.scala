package Domain

import cats.MonadError
import cats.instances.either._

case class Pizza private(cookingTime: Int) extends Order

object Pizza {

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  def apply(cookingTime: Int) =
    if (cookingTime > 0) Right(new  Pizza(cookingTime))
    else monadError.raiseError("OrderError")

}