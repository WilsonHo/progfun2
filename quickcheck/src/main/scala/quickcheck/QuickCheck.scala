package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(x, h)

  lazy val genMap: Gen[Map[Int, Int]] = for {
    k <- arbitrary[Int]
    v <- arbitrary[Int]
    m <- oneOf(const(Map.empty[Int, Int]), genMap)
  } yield m.updated(k, v)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  /**
    * Example
    */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /**
    * Example
    */
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /**
    * If you insert any two elements into an empty heap,
    * finding the minimum of the resulting heap should
    * get the smallest of the two elements back.
    */
  property("Check insert two elements into an empty heap and find the minimum") =
    forAll { (a1: Int, a2: Int) =>
    val h1 = insert(a1, empty)
    val h2 = insert(a2, h1)
    findMin(h2) == Math.min(a1, a2)
  }

  /**
    * If you insert an element into an empty heap, then
    * delete the minimum, the resulting heap should be empty.
    */
  property("Check insert a element into an empty heap and delete the minimum") =
    forAll { (a: Int) =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  /**
    * Given any heap, you should get a sorted sequence of
    * elements when continually finding and deleting minima.
    * (Hint: recursion and helper functions are your friends.)
    */
  property("Check sort elements") = forAll { (h1: H) =>
    def getSortedList(h: H): List[Int] =
      if (isEmpty(h)) Nil
      else findMin(h) :: getSortedList(deleteMin(h))

    getSortedList(h1) == getSortedList(h1).sortWith(_ < _)
  }

  /**
    * Finding a minimum of the melding of any two heaps should
    * return a minimum of one or the other.
    */
  property("Check find a minimum of the melding") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == Math.min(findMin(h1), findMin(h2))
  }

  property("Check find a minimum of the melding with empty head") = forAll { (h: H) =>
    findMin(meld(h, empty)) == findMin(h)
  }

  property("Check insert a element into an heap and delete the minimum") =
    forAll { (a: Int, h: H) =>
      val h1 = insert(a, h)
      val h2 = deleteMin(h1)
      val min = if (findMin(h) < a) findMin(deleteMin(h)) else findMin(h)
      findMin(h2) == min
    }
}
