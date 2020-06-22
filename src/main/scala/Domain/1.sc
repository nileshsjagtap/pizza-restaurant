

def sequence[A, B](list: List[Either[A, B]]): Either[List[A], List[B]] = {
  def iterate(remaining: List[Either[A, B]], buffer: Either[List[A], List[B]]): Either[A, List[B]] = {
    remaining match {
      case Nil =>  buffer
      case h :: t if h.isLeft => iterate(t, Left(buffer.left.get :+ h.left.get ))
      case h :: t => iterate(t, Right(buffer.right.get :+ h.right.get))
    }
  }
  iterate(list, Right(List.empty[B]))

}