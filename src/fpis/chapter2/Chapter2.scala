package fpis.chapter2

import scala.annotation.tailrec

object Chapter2 extends App {

  //2.1
  def fibonacci(n: Int): Int = {
    @tailrec
    def fib(n: Int, last: Int, current: Int): Int = {
      if (n <= 0) current
      else fib(n - 1, last + current, last)
    }

    fib(n, 1, 0)
  }

  println(fibonacci(5) == 5)
  println(fibonacci(8) == 21)

  //2.2
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(index: Int): Boolean = {
      if(index >= as.length - 1) true
      else ordered(as(index), as(index + 1)) && go(index + 1)
    }

    go(0)
  }

  val array = Array(1, 2, 3, 4)
  val array2 = Array(4, 3, 2, 1)
  def orderAsc(x: Int, y: Int): Boolean = x < y
  def orderDesc(x: Int, y: Int): Boolean = x > y
  println(isSorted(array, orderAsc) == true)
  println(isSorted(array2, orderDesc) == true)
  println(isSorted(array, orderDesc) == false)
  println(isSorted(array2, orderAsc) == false)

  def sum(x: Int, y: Int) = x + y
  //2.3
  def curry[A, B, C](f: (A, B) => C): A => B => C = (a: A) => (b: B) => f(a, b)
  def add7 = curry(sum)(7)
  println(add7(4) == 11)

  //2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  def multiply(x: Int)(y: Int) = x * y
  def uncurriedMultiply = uncurry(multiply)
  println(uncurriedMultiply(5,6) == 30)

  //2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
  def multiplyBy5 = multiply(5)(_)
  def multipliedSum = compose(multiplyBy5, add7)
  println(multipliedSum(3) == 50)
}
