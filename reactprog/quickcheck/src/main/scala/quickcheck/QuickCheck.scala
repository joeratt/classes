package quickcheck


import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll {
    a: A => {
      val h = insert(a, empty)
      findMin(h) == a
    }
  }

  property("min2") = forAll {
    (a: A, b: A) => {
      val h = insert(b, insert(a, empty))
      findMin(h) == ord.min(a, b)
    }
  }
/*
  property("min3") = forAll {
    a: A => {
      val h = insert(7,insert(1, insert(5, insert(3, empty))))
      findMin(deleteMin(deleteMin(h))) == 5
    }
  }*/

    property("OrderMaintainsRegardlessOfInsertOrder") = forAll {
      (a: A, b: A, c: A, d:A) => {
        def vals: List[A] = List(a,b,c,d)
        def sortedVals = vals.sortWith(ord.lt(_,_))
        val h = insert(sortedVals(sortedVals.size-1),insert(sortedVals.head,insert(sortedVals.tail.tail.head, insert(sortedVals.tail.head, empty))))
        val min1 = findMin(h)
        val h2 = deleteMin(h)
        val min2 = findMin(h2)
        val h3 = deleteMin(h2)
        val min3 = findMin(h3)
        val h4 = deleteMin(h3)
        val min4 = findMin(h4)
        def foundVals = List(min1,min2,min3,min4)
        sortedVals.equals(foundVals)
      }
    }

  property("actualDeletesMin") = forAll {
    (a: A, b: A) => {
      val h = insert(b, insert(a, empty))
      findMin(h) == ord.min(a, b) && findMin(deleteMin(h)) == ord.max(a, b)
    }
  }

    property("emptyThatHeap") = forAll {
      a: A => {
        val h = insert(a, empty)
        isEmpty(deleteMin(h))
      }
    }

  property("mindsForMelding") = forAll {
    (h1: H, h2: H) =>
      findMin(meld(h1, h2)) == ord.min(findMin(h1), findMin(h2))
  }

  //  def minVal(a: A, b: A): A = if (a > b) b else a

  def isProperPriorityQueue(heap: H, previousVal: A): Boolean = {
    if (isEmpty(heap)) true

    val nextMin: A = findMin(heap)
    if (nextMin < previousVal) false

    isProperPriorityQueue(deleteMin(heap), nextMin)
  }

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[A]
    m <- oneOf(value(empty), genHeap)
  } yield insert(k, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
