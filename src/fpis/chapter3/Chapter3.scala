package fpis.chapter3

import fpis.chapter3.Chapter3.List.{sum, tail, setHead, drop, dropWhile, init, lengthFoldRight, sumFoldLeft, productFoldLeft, lengthFoldLeft, appendFoldRight,
  concat, addOne, doubleToString, map, filter, flatMap, filterUsingFlatMap, addIntLists, zipWith, hasSubsequence}

import scala.annotation.tailrec


object Chapter3 extends App {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    //3.2
    def tail[A](as: List[A]): List[A] = as match {
      case Nil => Nil
      case Cons(_, t) => t
    }

    //3.3
    def setHead[A](as: List[A], h: A): List[A] = as match {
      case Nil => Cons(h, Nil)
      case Cons(_, xs) => Cons(h, xs)
    }

    //3.4
    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(_, xs) => if (n <= 0) l else drop(xs, n - 1)
    }

    //3.5
    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h, t) => Cons(h, append(t, a2))
      }

    //3.6
    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    @tailrec
    def dropWhileCurried[A](as: List[A])(f: A => Boolean): List[A] =
      as match {
        case Cons(h, t) if f(h) => dropWhileCurried(t)(f)
        case _ => as
      }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def sum2(ns: List[Int]): Int =
      foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]): Double =
      foldRight(ns, 1.0)(_ * _)

    //3.7
    //We can't. We first evaluate the whole list before applying function.

    //3.8
    //We will produce the exact same list as a result.

    //3.9
    def lengthFoldRight[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

    //3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      @tailrec
      def iter(as: List[A], acc: B): B = as match {
        case Nil => acc
        case Cons(x, xs) => iter(xs, f(acc, x))
      }
      iter(as, z)
    }

    //3.11.1
    def sumFoldLeft(as: List[Int]): Int = foldLeft(as, 0)((x,y) => x + y)

    //3.11.2
    def productFoldLeft(as: List[Int]): Int = foldLeft(as, 1)((x,y) => x * y)

    //3.11.3
    def lengthFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((n,_) => n + 1)

    //3.12
    def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())((b, a) => Cons(a, b))

    //3.13.1
    def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)

    //3.13.2
    def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((a, b) => f(b,a))

    //3.14
    def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((a, b) => Cons(a, b))

    //3.15
    def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil:List[A])(append)

    //3.16
    def addOne(as: List[Int]): List[Int] = foldRight(as, Nil:List[Int])((h,t) => Cons(h+1,t))

    //3.17
    def doubleToString(as: List[Double]): List[String] = foldRight(as, Nil:List[String])((h,t) => Cons(h.toString,t))

    //3.18
    def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil:List[B])((h, t) => Cons(f(h), t))

    //3.19
    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((h, t) => if(f(h)) Cons(h, t) else t)

    //3.20
    def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((h, t) => append(f(h), t))
    //alternative: def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

    //3.21
    def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) Cons(a, Nil) else Nil)

    //3.22
    def addIntLists(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addIntLists(t1, t2))
    }

    //3.23
    def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C) : List[C] = (l1, l2) match {
      case (_, Nil) => Nil
      case (Nil, _) => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }

    //3.24
    @tailrec
    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
      case _ => false
    }
  }


  //3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  val list = List(1, 2, 3, 4, 5)
  val sub = List(2, 3)
  val notSub = List(4, 3)
  val list2 = List(6, 7, 8, 9, 10)
  val list3 = List(1.1, 2.2, 3.3, 4.4, 5.5, 1.1)

  println(tail(list))
  println(setHead(list, 0))
  println(drop(list, 3))
  println(dropWhile[Int](list, x => x < 3))
  println(init(list))
  println(lengthFoldRight(list))
  println(sumFoldLeft(list))
  println(productFoldLeft(list))
  println(lengthFoldLeft(list))
  println(appendFoldRight(list, list2))
  println(concat(List(list, list2, list, list2)))
  println(addOne(list))
  println(doubleToString(list3))
  println(map(list3)(_ + 2))
  println(filter(list3)(_ < 3))
  println(flatMap(list)(i => List(i,i)))
  println(filterUsingFlatMap(list3)(_ < 3))
  println(addIntLists(list, list2))
  println(zipWith(list, list2)(_ + _))
  println(hasSubsequence(list, sub))
  println(hasSubsequence(list, notSub))
}
