package Core

trait Traverse2[F[_]] {

  def traverse2[A,B](list: List[Either[A,B]]): Either[A, List[B]]

}

object Traverse2{

  def apply[A](implicit t: Traverse2[A]) = t

  implicit def traverseEither[A,B] = new Traverse2[Either[A,B]] {
    override def traverse2[A, B](list: List[Either[A, B]]): Either[A, List[B]] = {

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

  implicit def traverseEither[A,B] = new Traverse2[Option[A,B]] {

  implicit class Traverse2Ops[F[A,B],A, B](src: F[A, B]){
    def traverse(list: List[F[A, B]])(implicit t: Traverse2[F]): F[A, List[B]] = t.traverse2[A,B](list)
  }
}




