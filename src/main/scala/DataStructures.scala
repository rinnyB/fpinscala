package fpinscala.list

import scala.annotation.tailrec



sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if (as isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => throw new IllegalArgumentException
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => throw new IllegalArgumentException
    case Cons(_, xs) => Cons(newHead, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(x, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else xs
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => throw new IllegalArgumentException
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))

    }
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z // zero element for f op
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)(_ + _)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1D)(_ * _)
  }

  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, x) => x + 1)
  }

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
  as match {
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    case Nil => z
  }

  def sum3(ns: List[Int]): Int = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Double]): Double = {
    foldLeft(ns, 1D)(_ * _)
  }

  def length3[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((x, y) => Cons(y, x))
  }

  def append[A](as: List[A], appendix: List[A]): List[A] = {
    foldRight(as, appendix)(Cons(_, _))
  }

  def concat[A](as: List[List[A]]): List[A] = {
    foldRight(as, Nil:List[A])(append)
  }

  def addOne(ns: List[Int]): List[Int] = {
    foldRight(ns, Nil:List[Int])((x, y) => Cons(x + 1, y))
  }

  def stringify(ns: List[Double]): List[String] = {
    foldRight(ns, Nil:List[String])((x, y) => Cons(x.toString, y))
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil: List[B])((x, y) => Cons(f(x), y))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil: List[A]){
      (x, y) => if (f(x)) Cons(x, y) else y
    }
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if (f(a)) List(a) else Nil)
  }

  def zipInts(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipInts(t1, t2))
  }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }




}

object foo {
  def main(args: Array[String]): Unit = {
    val vx = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val vz = List(1D, 2D, 3D, 4D, 5D, 6D, 7D, 8D, 9D, 10D)
    val cx = List.zipWith(vx, vz)(_ + _)
    println(cx)
  }
}
