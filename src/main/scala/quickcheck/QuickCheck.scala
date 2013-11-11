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
    isEmpty(h2) == true
  }

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    heap <- oneOf(value(empty), genHeap)
  } yield insert(x, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
