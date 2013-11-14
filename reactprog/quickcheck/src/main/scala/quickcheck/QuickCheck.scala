package quickcheck


import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("OrderMaintainsRegardlessOfInsertOrder") = forAll {
    (a: A, b: A, c: A, d: A) => {
      val h = insert(a, insert(b, insert(c, insert(d, empty))))
      val values: List[A] = getPriorityQueueValues(h)
      findMin(h) == ord.min(a,ord.min(b,ord.min(c,d))) && List(a, b, c, d).forall (values contains)
      }
  }

  def getPriorityQueueValues(h: H): List[A] = {
    if (isEmpty(h)) Nil
    else findMin(h) :: getPriorityQueueValues(deleteMin(h))
  }

  lazy val genHeap: Gen[H] = for {
    v <- arbitrary[A]
    h <- oneOf(value(empty), genHeap)
  } yield insert(v,h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
