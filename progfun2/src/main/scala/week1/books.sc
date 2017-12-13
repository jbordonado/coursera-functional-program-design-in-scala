object books {
  case class Book(title: String, authors: List[String])

  //A mini-database - implemented as a List - BAD
  val booksList = Set(
    Book(
      title = "Structure and Interpreation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(
      title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(
      title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(
      title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  //A mini-database - implemented as a Set - GOOD
  val booksSet = Set(
    Book(
      title = "Structure and Interpreation of Computer Programs",
      authors = List("Abelson, Harald", "Sussman, Gerald J.")),
    Book(
      title = "Introduction to Functional Programming",
      authors = List("Bird, Richard", "Wadler, Phil")),
    Book(
      title = "Effective Java",
      authors = List("Bloch, Joshua")),
    Book(
      title = "Java Puzzlers",
      authors = List("Bloch, Joshua", "Gafter, Neal")),
    Book(
      title = "Programming in Scala",
      authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )

  // Get books that Joshua Bloch wrote
  for (b <- booksList; a <- b.authors if a startsWith "Bloch,") yield b.title
  for (b <- booksSet; a <- b.authors if a startsWith "Bloch,") yield b.title

  //To find all the books which have the word "Program" in the title
  for(b <- booksList if b.title.indexOf("Program") >= 0) yield b.title
  for(b <- booksSet if b.title.indexOf("Program") >= 0) yield b.title
  booksSet.map(b => b.title).filter(title => title.indexOf("Program") >= 0)

  // Translating for comprehension into higher-order functions
  for(b <- booksSet; a <- b.authors if a startsWith "Bloch,") yield b.title
  // Step 1 : substitute the first generator by flatMap
  booksSet flatMap(b =>
    for(a <- b.authors if a.startsWith("Bloch,")) yield b.title)
  // Step 2 : substitute the if-statement by withFilter
  booksSet flatMap(b =>
    for(a <- b.authors withFilter (a => a.startsWith("Bloch,"))) yield b.title)
  // Step 3 : substitute the second generator by map
  booksSet flatMap(b =>
    b.authors withFilter (a => a.startsWith("Bloch,")) map (_ => b.title))

  // Get the name of all authors who have written at least two books in the 'books database'
  for {
    b1 <- booksList
    b2 <- booksList
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1

  for {
    b1 <- booksSet
    b2 <- booksSet
    if b1 != b2
    a1 <- b1.authors
    a2 <- b2.authors
    if a1 == a2
  } yield a1



}