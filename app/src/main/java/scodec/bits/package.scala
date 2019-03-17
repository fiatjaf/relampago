package scodec


package object bits {
  private[bits] implicit class EitherOps[L, R](val self: Either[L, R]) extends AnyVal {
    def map[R2](f: R => R2): Either[L, R2] = self match {
      case Right(r) => Right(f(r))
      case l: Left[L, R] => l.asInstanceOf[Either[L, R2]]
    }

    def flatMap[R2](f: R => Either[L, R2]): Either[L, R2] = self match {
      case Right(r) => f(r)
      case l: Left[L, R] => l.asInstanceOf[Either[L, R2]]
    }
    
    def toOption: Option[R] = self match {
      case Right(r) => Some(r)
      case Left(_) => None
    }
  }
}
