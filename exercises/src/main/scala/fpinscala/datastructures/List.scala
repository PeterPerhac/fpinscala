package fpinscala.datastructures

sealed trait List[+A]

// `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object App {

  import  List._

  def main(args: Array[String]): Unit = {
    val list = zipWith(List(1, 2, 3, 4, 5), List("Peter", "Tomas", "Andrej"))(_.toString.concat("_").concat(_))
    println(list)
  }

}

object List { // `List` companion object. Contains functions for creating and working with lists.

  def ofLength(n:Int) = (1 to n).foldLeft(Nil:List[Int]) {(list,el) => Cons(el, list)}

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }


  def append[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append2(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

  def product(l: List[Int]) = foldLeft(l, 1)(_ * _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail on a Nil list!")
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("tail on a Nil list!")
    case Cons(_, tail) => Cons(h, tail)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    n match {
      case _ if n <= 0 => l
      case _ => drop(tail(l), n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init on a Nil list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldLeft(l, 0)((a,le)=>a+1)

  @annotation.tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def reverse[A](l:List[A]):List[A]=foldLeft(l, Nil:List[A])((acc,e) => Cons(e,acc))

  def concat[A](ls: List[List[A]]) : List[A] = foldLeft(ls, Nil:List[A])(append)

  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((e, acc) => Cons(f(e), acc))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if (f(h)) Cons(h, t) else t)

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(e => if (f(e)) List(e) else Nil)

  def zipWith[A, B, C](l1: List[A], l2: List[B])(f: (A, B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}
