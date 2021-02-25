package fpis.chapter5

import fpis.chapter5.Chapter5.Stream.{cons, empty}

object Chapter5 extends App {

  sealed trait Stream[+A] {
    //5.1
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List()
    }

    //5.2.1
    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case Cons(h, _) if n == 1 => cons(h(), empty)
      case _ => empty
    }

    //5.2.2
    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case _ => this
    }

    //5.3
    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
      case _ => empty
    }

    def exists(p: A => Boolean): Boolean = this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def existsFoldRight(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

    //5.4
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    //5.5
    def takeWhileFoldRight(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) =>
        if (f(h)) cons(h, t)
        else empty)

    //5.6
    def headOption: Option[A] = foldRight(None: Option[A])((h, _) => Some(h))

    //5.7.1
    def mapFoldRight[B](f: A => B): Stream[B] = foldRight(empty[B])((h, t) => cons(f(h), t))

    //5.7.2
    def filterFoldRight(f: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

    //5.7.3
    def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s)((h, t) => cons(h, t))

    //5.7.4
    def flatMapFoldRight[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

    def find(p: A => Boolean): Option[A] = filterFoldRight(p).headOption

    //5.8
    def constant[A](a: A): Stream[A] = cons(a, constant(a))

    //5.9
    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    //5.10
    def fibs: Stream[Int] = {
      def go(x: Int, y: Int): Stream[Int] = cons(x, go(y, x + y))

      go(0, 1)
    }

    //5.11
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((h, t)) => cons(h, unfold(t)(f))
      case None => empty
    }

    //5.12.1
    def fibsUnfold: Stream[Int] = unfold((0, 1))(p => p match {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    })

    def fibsUnfoldShort: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1))) }

    //5.12.2
    def fromUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

    //5.12.3
    def constantUnfold(n: Int): Stream[Int] = unfold(n)(n => Some((n, n)))

    //5.12.4
    def onesUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))


  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
