package redbook.state

package object v2 {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  val nonNegativeInt: Rand[Int] = map(int) { i =>
    if (i < 0)
      i - Int.MinValue
    else i
  }

  val double: Rand[Double] = map(nonNegativeInt) { n =>
    n.toDouble / (Int.MaxValue + 1L)
  }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rs.foldLeft(unit(List.empty[A])) { (acc: Rand[List[A]], r: Rand[A]) =>
      map2(r, acc)(_ :: _)
    }

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

}
