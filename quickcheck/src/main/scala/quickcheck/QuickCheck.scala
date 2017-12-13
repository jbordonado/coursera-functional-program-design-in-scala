package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  // if you insert an element into an empty heap, then find the minimum of the resulting heap, you get the element back
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  // If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back
  property("hint1") = forAll { (a: Int, b: Int) =>
    val n = insert(b, insert(a, empty))
    if (a <= b) {
      findMin(n) == a
    }
    else {
      findMin(n) == b
    }
  }

  // If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty
  property("hint2") = forAll { (a: Int) =>
    deleteMin(insert(a, empty)) == empty
  }

  // Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima
  property("hint3") = forAll { (h: H) =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val min = findMin(h)
        val newHeap = deleteMin(h)
        isEmpty(newHeap) || min <= findMin(newHeap) && isSorted(newHeap)
      }
    }
    isSorted(h)
  }

  // Finding a minimum of the melding of any two heaps should return a minimum of one or the other
  property("hint4") = forAll { (h1: H, h2: H) =>
    val min1 = if (isEmpty(h1)) 0 else findMin(h1)
    val min2 = if (isEmpty(h2)) 0 else findMin(h2)
    val mergedHeap = meld(h1, h2)
    if (isEmpty(mergedHeap)) {
      true
    }
    else {
      findMin(mergedHeap) == min1 || findMin(mergedHeap) == min2
    }
  }

  // Take two arbitrary heaps, meld together. Then remove min from 1 and insert it into 2, meld the results. Compare two melds by comparing sequences of ranks.
  property("meld") = forAll { (h1: H, h2: H) =>
    def heapEqual(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
      }
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }
}
