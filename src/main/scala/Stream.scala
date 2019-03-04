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
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight(None: Option[A])((h, _) => Some(h))
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B])((a, b) => cons(f(a), b))
  }
  
  def filterViaFoldRight(p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  // what is >:
  def append[B >: A](s: => Stream[B]): Stream[B] = 
    foldRight(s)((h, t) => cons(h, t))


  def flatMapViaFoldRight[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B])((h, t) => f(h) append t)
  }
  
  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this)(state => state match {
      case Cons(h, t) => Some(( f(h()), t() )) //wtf
      case _ => None
    })
  }

  // def takeViaUnfold(n: Int): Stream[A] = {
  //   unfold((this, n)){
  //     case (state, num) => (state, num) match {
  //       case (Cons(h, t), 1) => Some((h(), (empty, 0)))
  //       case (Cons(h, t), n) if n > 1 => Some ((h(), (t(), n - 1)))
  //       case _ => None
  //     }      
  //   }
  // }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some ((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }
  }

  def zipWithViaUnfold[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, other)){
      case (Cons(h1, t1), Cons(h2, t2)) => Some(
        (f(h1(), h2()), (t1(), t2()))
      )
      case _ => None
    }
  }

  /** black magic ###ery
    */
  def zipAllViaUnfold[B, C](other: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
    unfold((this, other)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((f(Some(h1()), Option.empty[B]), (t1(), empty[B])))
      case (Empty, Cons(h2, t2)) => Some((f(Option.empty[A], Some(h2())), (empty[A], t2())))
      case (Empty, Empty) => None
    }
  }


  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    zipAllViaUnfold(s2)((_,_))


  // more bmf >_>
  def startsWith[A](other: Stream[A]): Boolean = {
    // this.zipAllViaUnfold(other)((a, b) => a == b).forAll(_ == true)
    zipAll(other).takeWhile(_._2.nonEmpty) forAll {
      case (a, b) => a == b
    }
  }

  // def tailsViaUnfold: Stream[Stream[A]] = {
  //   unfold(this) {
  //     case Cons(h, t) => Some((cons(h(), t()), t()))
  //     case Empty => None
  //   }
  // }

  // ...
  def tailsViaUnfold: Stream[Stream[A]] = 
    unfold(this) {
      case Empty => None     
      case s => Some((s, s drop 1))
    } append Stream(empty)
  

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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }
  
  def from(n: Int): Stream[Int] = {
    Cons(() => n, () => from(n + 1))
  }

  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = cons(a, go(b, a + b))
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => cons(a, unfold(s)(f))
    case None => empty    
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a)(state => Some((state, state)))
  }

  def fibsViaUnfold: Stream[Int] = {
    // unfold((0, 1))(state => Some(state._2, (state._2, state._1 + state._2)))
    unfold((0, 1)) {
      case (a, b) => Some(b, (b, a + b))
    }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(state => Some((state + 1, state + 1)))
  }

  def onesViaUnfold: Stream[Int] = {
    unfold(1)(_ => Some(1, 1))
  }


}

object Test extends App {

  val a = Stream(1, 2, 3, 4, 5)
  a.tailsViaUnfold.map(_.toList.map(e=>e.toString)).map("(" + _.mkString(",") + ")").toList.foreach(println)
}


