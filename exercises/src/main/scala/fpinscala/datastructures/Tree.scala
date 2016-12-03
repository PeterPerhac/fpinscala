package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object App2 {

  import Tree._

  def main(args: Array[String]): Unit = {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
    val mapped = map(tree)(_ + 1)
    println(mapped)
  }
}

object Tree {


  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)
  }


  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(value) => 0
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }


  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(n => 1)(1 + _ + _)

  def maximumViaFold(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(n => 0)((d1, d2) => 1 + (d1 max d2))

  def mapViaFold[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

}