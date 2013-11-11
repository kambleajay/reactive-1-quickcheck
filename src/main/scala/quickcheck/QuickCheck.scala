package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (x: Int, y: Int) =>
    val h = insert(y, insert(x, empty))
    findMin(h) == (x min y)
  }

  property("delMin") = forAll { x: Int =>
    val h1 = insert(x, empty)
    val h2 = deleteMin(h1)
    isEmpty(h2)
  }

  property("order of minimas") = forAll { heap: H =>
    val minimas = minimasOf(heap)
    val minimasSorted = minimas.sorted
    minimas == minimasSorted
  }

  def minimasOf(heap: H): List[Int] = {
    if(isEmpty(heap)) Nil
    else findMin(heap) :: minimasOf(deleteMin(heap))
  }

  property("min of melds") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val mergedHeap = meld(h1, h2)
    val minOfMerged = findMin(mergedHeap)
    (minOfMerged == min1) || (minOfMerged == min2)
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m1 = meld(h1, h2)
    val h1Min = findMin(h1)
    val h1WithoutMin = deleteMin(h1)
    val h2WithMin = insert(h1Min, h2)
    val m2 = meld(h1WithoutMin, h2WithMin)
    eqHeap(m1, m2)
  }

  def eqHeap(h1: H, h2: H): Boolean = {
    if(isEmpty(h1) && isEmpty(h2)) true
    else if(isEmpty(h1) || isEmpty(h2)) false
    else {
      val h1Min = findMin(h1)
      val h2Min = findMin(h2)
      if(h1Min != h2Min) false
      else eqHeap(deleteMin(h1), deleteMin(h2))
    }
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    heap <- oneOf(value(empty), genHeap)
  } yield insert(x, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
