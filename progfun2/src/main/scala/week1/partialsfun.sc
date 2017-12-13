object funs {

  /** Partial matches **/

  val f: String => String = { case "ping" => "pong" }
  f("ping")
  //The following will give a match error because we don't have a case of "abc"
  //f("abc")

  val f1: PartialFunction[String, String] = { case "ping" => "pong" }
  f1.isDefinedAt("ping")
  f1.isDefinedAt("pong")
  f1("ping")

  List("ping", "abc", "pong").map(a =>
    if(f1.isDefinedAt(a)) f1(a) else "404"
  )

  val f2: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: y :: rest => "two"
  }

  f2.isDefinedAt(List(1, 2, 3))

  val g: PartialFunction[List[Int], String] = {
    case Nil => "one"
    case x :: rest => rest match {
      case Nil => "two"
    }
  }

  g.isDefinedAt(List(1, 2, 3))
  //below code gives you a match error isDefined doesn't protect you from match error
  //g(List(1,2, 3))

  val f3: (Int => String) = List("a", "b", "c")
  f3(2)
}