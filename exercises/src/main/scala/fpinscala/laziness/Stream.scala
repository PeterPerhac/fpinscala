package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = foldRight(Nil:List[A]){ _ :: _ }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Cons(h, t) => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def forAll(p: A => Boolean): Boolean = !exists(i => !p(i))

  def headOption: Option[A] = foldRight(None:Option[A])((a:A,_) => Some(a))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object TestApp {

  def main(args: Array[String]): Unit = {

    val s = Stream(1, 2, 3, 4, 5)
    println(s.toList)
    println((s take 2).toList)
    println((s drop 2).toList)
    println((s takeWhile (n => n < 4)).toList)

    val prints = Stream(() => {
      println("hello")
    }, () => {
      println("world")
    }, () => {
      do {
        ;
      } while (true)
    })
    prints.take(2).toList.foreach(invoke => invoke())

    assert(Stream(1, 2, 3, 4, 5, 6, 9).forAll(_ < 10))

    //    ones.forAll(_ < 10)
    assert(Stream(1, 2, 3, 4, 5).headOption contains 1)
    assert(Stream().headOption.isEmpty)

    assert((Stream.constant("Peter") take 2).toList == List("Peter", "Peter"))

    assert((Stream.from(1) take 5).toList == (1 to 5).toList)

  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = Cons(() => hd, () => tl)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](v:A):Stream[A] = Stream.cons(v, constant(v))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")
}