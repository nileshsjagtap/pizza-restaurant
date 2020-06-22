package Domain

import Core.Traverse
import cats.MonadError
import cats.instances.either._
import cats.syntax.monadError

case class Customers(customers: List[Customer]) {

  def avgWaitingTime: Int = {
    val sortedCustomers = customers.sortWith(_.order.cookingTime < _.order.cookingTime).reverse

    def iterate(list: List[Customer]): List[Int] = {

      def sequence(list: List[Customer], res : List[Int]): List[Int] = {
        list match {
          case Nil => res
          case h :: Nil => sequence(Nil, res :+ h.order.cookingTime - h.entryTime)
          case h :: t => sequence(t, res :+ h.order.cookingTime + t.map(_.order.cookingTime).sum - h.entryTime)
        }
      }

      sequence(list, List.empty[Int])
    }

    val intermedidateRes = iterate(sortedCustomers).sum
    intermedidateRes/sortedCustomers.size
  }

}

object Customers {

  type ErrorOr[A] = Either[String, A]
  type monadError[F[_]] = MonadError[F, String]

  def apply(list: List[List[Int]]): Either[String, Customers] = {

    val res: List[Either[String, Customer]] = list.map(ip => for {
      order <- Pizza(ip(1))
      customer <- Customer(ip(0), order)
    } yield customer)

    Traverse[String, Customer].traverse(res)
      .fold( e => monadError.raiseError(e), customerList =>  Right(Customers(customerList)))

//    val rrr = Traverse2[Either].traverse2(res)


  }

}
