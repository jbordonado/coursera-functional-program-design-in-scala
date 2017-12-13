object primes {
  def from(n: Int): Stream[Int] = n #:: from(n+1)

  val nats = from(0)
  val m4s = nats map (_ * 4)

  m4s take 100 toList

  def sieves(s: Stream[Int]): Stream[Int] = s.head #:: sieves(s.tail filter (_ % s.head != 0))

  val primes = sieves(from(2))

  primes take(100) toList

  def sqrtStream(x: Double): Stream[Double] = {
    def improve(guess: Double) = (guess + x / guess) / 2
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }

  sqrtStream(4).take(10).toList

  def isGoodEnough(guess: Double, x: Double) = math.abs((guess * guess - x) / x) < 0.0001

  sqrtStream(4).filter(isGoodEnough(_, 4)).take(10).toList
}