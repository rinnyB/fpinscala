package fpinscala.tree

import Tree._

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object Stump extends Tree[Nothing]


object Tree {


  def count[A](tree: Tree[A]): Int = tree match {

    case Branch(left, right) => 1 + count(left) + count(right)
    case Leaf(_) => 1
    case Stump => 0

  }

  def max(tree: Tree[Int]): Int = tree match {
    case Branch(left, right) => max(left) max max(right)
    case Leaf(value) => value
    case Stump => Int.MinValue
  }

  def depth[A](tree: Tree[A], soFar: Int): Int = tree match {
    case Branch(left, right) => depth(left, soFar + 1) max depth(right, soFar + 1)
    case Leaf(_) => soFar
    case Stump => soFar - 1
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Branch(left: Tree[A], right: Tree[A]) => Branch(map(left)(f), map(right)(f))
    case Leaf(a) => Leaf(f(a))
    case Stump => Stump
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B)(implicit default: B): B = tree match {
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    case Leaf(a) => f(a)
    case Stump => default
  }
}

object Test extends App {
  val tree =
    Branch (
      Leaf(1),
      Branch(
        Branch(
          Leaf(-10), Branch(Stump, Leaf(1))),
          Leaf(1)
      )
  )
  println(count(tree))
  println(depth(tree, 0))
  println(tree)
  println(fold(tree)(e => e.toDouble)(_ + _)(0))
  print(max(tree))
}