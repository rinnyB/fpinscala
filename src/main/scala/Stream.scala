package fpinscala.stream

import Stream._

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    @tailrec
    def go(stream: Stream[A], res: List[A]): List[A] = stream match {
      case Empty => res
      case Cons(h, t) => go(t(), h() :: res)
    }
    go(this, Nil).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, t) if n == 1 => cons(h(), Empty)
    case _ => empty
  }

  /**
    *
    * @param n number of elements to drop
    * @return
    */
  def drop(n: Int): Stream[A] = {
    def go(iter: Int, stream: Stream[A]): Stream[A] = stream match {
      case Cons(_, t) if iter > 0 => go(iter - 1, t())
      case Cons(_, _) if iter == 0 => stream
      case Empty => stream
    }
    go(n, this)
  }

  def drop2(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >= 0 => t().drop(n - 1)
    case _ => this
  }

  //requires reverse call;
//  def takeWhile(p: A => Boolean): Stream[A] = {
//    def go(stream: Stream[A], acc: Stream[A]): Stream[A] = stream match {
//      case Cons(h, t) if p(h()) => go(t(), cons()
//      case _ => acc
//    }
//    go(this, Empty)
//  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) && b)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight(None[A])((h, _) => Some(h))
  }

}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}

object Test extends App {
  val stream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6, 7, 8, 9)
//  stream.drop(3).toList.foreach(println)
//  stream.drop2(3).toList.foreach(println)

  stream.takeWhile(_ < 3).toList.foreach(println)
}


