package fpinscala.state

import fpinscala.state.RNG.Simple


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text



  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def string: Rand[String] = map(int)("chuj" + _)

  def unit[A](a: A): Rand[A] = rng => (a, rng)


  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }



    def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1) , r)
  }

  def double_ : Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, r2) = double(r)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1,d2,d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldRight((Nil: List[Int], rng)) { case (_, (l, r)) =>
      val (i, newR) = r.nextInt
      (i :: l, newR)
    }
  }

  def map[S, A, B](sf: S => (A,S))(f: A => B): S => (B, S) = so => {
    val (a, rng2) = sf(so)
    (f(a), rng2)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rc => {
    val (a, r1) = ra(rc)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(Nil:List[A]))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, r) = f(rng)
    g(a)(r)
  }

  def nonNegativeLessThan(n: Int): Rand[Int] = rng => {
    val dbl = double(rng)
    ((dbl._1 * n).toInt, dbl._2)
  }

  def mapViaFlatMap[A,B](a: Rand[A])(f: A => B): Rand[B] =
    flatMap(a)(aa => unit(f(aa)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => flatMap(rb)(b => unit(f(a, b))))

}

object MainTheMainMain {
  def main(args: Array[String]): Unit = {
    val rnd = Simple(42)
    RNG.ints(10)(rnd)._1 foreach println
    println("====")
    RNG.ints(10)(rnd)._1 foreach println

    println("====")
    println(RNG.double3(rnd)._1)
    println("====")
    val tuple = RNG.int(rnd)
    println(tuple._1)
    println(RNG.double(tuple._2)._1)
    println(RNG.map2(RNG.int, RNG.double)("" + _ + _)(rnd)._1)
    println(RNG.map2ViaFlatMap(RNG.int, RNG.double)("" + _ + _)(rnd)._1)

    println("===***===")
    val list: List[Any] = RNG.sequence(List(RNG.int, RNG.double_, RNG.double_, RNG.string))(rnd)._1
    list.foreach(println)

    println("===***===")
    val randomUnderTen = RNG.nonNegativeLessThan(10)(rnd)._1
    println(RNG.sequence(List.fill(1000)(RNG.nonNegativeLessThan(1000000)))(rnd)._1.map(_.toString).mkString(", "))

  }
}


case class State[S, +A](run: S => (A, S)) {

  def map[B](f: A => B): State[S, B] = sys.error("todo")

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = sys.error("todo")

  def flatMap[B](f: A => State[S, B]): State[S, B] = sys.error("todo")

}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
