package Core

import Domain.{Customer}
import cats.MonadError
import cats.instances.either._

trait Traverse[A,B] {
  def traverse(list: List[Either[A,B]]): Either[A, List[B]]
}

object Traverse{

  type ErrorOr[A] = Either[String, A]
  val monadError = MonadError[ErrorOr, String]

  def apply[A,B](implicit t: Traverse[A,B]) = t

  implicit def errorListListInt[A, B] = new Traverse[String, List[Int]] {
    override def traverse(list: List[Either[String, List[Int]]]): Either[String, List[List[Int]]] =
      sequence(list)
  }

  implicit def customerTraverse[A, B] = new Traverse[String, Customer] {
    override def traverse(list: List[Either[String, Customer]]): Either[String, List[Customer]] = {
      sequence(list)
    }
  }

  private def sequence[A, B](list: List[Either[A, B]]): Either[A, List[B]] = {
    def iterate(remaining: List[Either[A, B]], buffer: Either[A, List[B]]): Either[A, List[B]] = {
      remaining match {
        case Nil =>  buffer
        case h :: t if h.isLeft => Left(h.left.get)
        case h :: t => iterate(t, Right(buffer.right.get :+ h.right.get))
      }
    }
    iterate(list, Right(List.empty[B]))

  }


}
