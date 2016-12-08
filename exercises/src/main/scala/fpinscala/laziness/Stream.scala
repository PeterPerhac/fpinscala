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

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] = foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]):Stream[B] = foldRight(s)((h,t) => cons(h, t))

  def flatmap[B](f: A => Stream[B]) : Stream[B] = foldRight(empty[B])((h,t) => f(h) append t )

  //via unfold
  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), 1) => Some((h(), (empty[A], 0)))
    case (Cons(h, t), c) if c > 1 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }
  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)){
    case (Cons(h,t), Cons(h2,t2)) => Some((f(h(), h2()), (t(), t2())))
    case _ => None
  }

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object TestApp {

  def main(args: Array[String]): Unit = {

    val s = Stream(1, 2, 3, 4, 5)
    println(s.toList)
    println((s take 2).toList)
    println("*")
    println((s takeViaUnfold  2).toList)
    println((s drop 2).toList)
    println((s takeWhile (n => n < 4)).toList)
    println((s takeWhileViaUnfold  (n => n < 4)).toList)

    val prints = Stream(() => { println("hello")}, () => { println("world")}, () => { do { ;} while (true)})
    prints.take(2).toList.foreach(invoke => invoke())

    assert(Stream(1, 2, 3, 4, 5, 6, 9).forAll(_ < 10))
    //    ones.forAll(_ < 10)
    assert((ones take 10).toList.sum == 10)
    assert((onesViaUnfold take 10).toList.sum == 10)
    assert(Stream(1, 2, 3, 4, 5).headOption contains 1)
    assert(Stream().headOption.isEmpty)

    assert(Stream(1,2,3,4,5,6).filter(_>=5).toList == List(5,6))
    assert(Stream(1,2).map(_.toString.concat("foo")).toList == List("1foo", "2foo"))
    assert(Stream(1,2).mapViaUnfold(_.toString.concat("foo")).toList == List("1foo", "2foo"))
    assert((Stream.constant("Peter") take 2).toList == List("Peter", "Peter"))
    assert((Stream.from(1) take 5).toList == (1 to 5).toList)
    assert((Stream.unfold(1)(i => Some((i, i+2))) take 5).toList == List(1,3,5,7,9))
    assert((Stream.fibs take 6).toList == List(0, 1,1,2,3,5))

    (fibViaUnfold take 10).toList.foreach( println )
    (constantViaUnfold(10) take 5).toList.foreach( println )
    println("=====")
    (fromViaUnfold(10) take 5).toList.foreach( println )
    println("=====")
    (fromViaUnfold(1) take 5).append(fibViaUnfold take 5).toList.foreach( println )

    println("=====")
    Stream(1, 2, 3).flatmap(n => Stream(n - 1, n, n + 1)).toList.foreach(print)
    println()

    println("somar")
    val stream = from(1).zipWith(Stream("Peter", "Tomas", "Andrej", "Bobor", "Fafek", "Sumichrast" ))(_.toString.concat("_").concat(_))
    (stream take 4 ).toList.foreach(println)


  }
}

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = Cons(() => hd, () => tl)

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant[A](v:A):Stream[A] = Stream.cons(v, constant(v))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs : Stream[Int] = {
    def next(prev:Int, curr:Int) : Stream[Int] = cons(prev, next(curr, curr+prev))
    next(0,1)
  }

  def fibViaUnfold: Stream[Int] = unfold((0,1)) { case (prev,curr) => Some((prev, (curr, prev+curr )))}

  def constantViaUnfold[A](v:A): Stream[A] = unfold(v)(_ => Some((v,v)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(nn => Some((nn, nn + 1)))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a,s)) => cons(a, unfold(s)(f))
    case None => empty
  }
}