object collections {
  List(1,2,3).map(_ + 1)
  List(1,2,3).flatMap(x => List(x,x))
  List(1,2,3) zip List("a","b","c")
  List(1,2,3,4).partition(_ < 3)
  List(1,2,3,4).filter(_ % 2 == 1)

  //For-expressions or for comprehension - useful for when you want to do nested loops

  //instead of
  def isPrime(n: Int) = n % 2 == 0
  val n = 10
  (1 until n) flatMap(i =>
    (1 until i) withFilter (j => isPrime(i + j)) map
      (j => (i, j)))

  //with for expression
  for {
    i <- 1 until n //a generator
    j <- 1 until i //another generator
    if isPrime(i+j)
  } yield(i,j)

  /* Map, flatMap, and filter implemented using for expression
   * but in reality, scala compiler translates for expressions in terms of
   * map, flatMap, and a lazy variation of filter (withFilter)
   */
  def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    for(x <- xs) yield f(x)

  def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] =
    for(x <- xs; y <- f(x)) yield y

  def filter[T](xs: List[T], p: T => Boolean): List[T] =
    for(x <- xs if p(x)) yield x
}