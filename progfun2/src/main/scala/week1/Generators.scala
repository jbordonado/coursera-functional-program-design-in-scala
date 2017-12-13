package week1

object Generators {
    trait Generator[+T] {
      self => // an alias for "this"
      def generate: T
      def foreach[U](f: T => U) {
        f(generate)
      }
      def map[S](f: T => S): Generator[S] = new Generator[S] {
        def generate = f(self.generate)  // also can be written "Generator.this.generate" or "this.generate"
      }
      def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
        def generate = f(self.generate).generate
      }
    }

    trait Tree
    case class Inner(left: Tree, right: Tree) extends Tree
    case class Leaf(x: Int) extends Tree
}
