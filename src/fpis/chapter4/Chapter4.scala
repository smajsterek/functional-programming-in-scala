package fpis.chapter4

object Chapter4 extends App {

  sealed trait Option[+A] {

    //4.1.1
    def map[B](f: A => B): Option[B] = this match {
      case Some(x) => Some(f(x))
      case None => None
    }

    //4.1.2
    def getOrElse[B >: A](default: => B): B = this match {
      case Some(x) => x
      case None => default
    }

    //4.1.3
    def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

    //4.1.4
    def orElse[B >: A](ob: => Option[B]): Option[B] = this.map(Some(_)) getOrElse (ob)

    //4.1.5
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(a) if f(a) => this
      case _ => None
    }

    def mean(xs: Seq[Double]): Option[Double] =
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)

    //4.2
    def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))

    //4.3
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
      case (Some(sa), Some(sb)) => Some(f(sa, sb))
      case _ => None

    }

    //4.4
    def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
      case h :: t => h flatMap (h2 => sequence(t) map (h2 :: _))
      case Nil => Some(Nil)
    }

    //4.5.1
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
      case Nil => Some(Nil)
    }

    //4.5.2
    def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  sealed trait Either[+E, +A] {

    //4.6.1
    def map[B](f: A => B): Either[E, B] = this match {
      case Right(x) => Right(f(x))
      case Left(x) => Left(x)
    }

    //4.6.2
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

    //4.6.3
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    //4.6.4
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        a <- this
        b1 <- b
      } yield f(a, b1)

    //4.7.1
    def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = es match {
      case Nil => Right(Nil)
      case h :: t => (f(h) map2 traverse(t)(f)) (_ :: _)
    }

    //4.7.2
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

}
