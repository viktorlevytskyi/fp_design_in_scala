package quickcheck

import org.scalacheck.Arbitrary._
import org.scalacheck.Gen._
import org.scalacheck.Prop._
import org.scalacheck._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[A]
    h <- oneOf[H](genHeap, empty)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("hint1") = forAll { (a: A, b: A) =>
    val min = math.min(a, b)
    findMin(insert(a, insert(b, empty))) == min
  }

  property("hint2") = forAll { (a: A) =>
    isEmpty(deleteMin(insert(a, empty)))
  }

  property("hint3") = forAll { (h: H) =>
    def heapSequence(heap: H):List[A] = {
      if (isEmpty(heap)) Nil
      else findMin(heap) :: heapSequence(deleteMin(heap))
    }
    val sortedHeapSequence = heapSequence(h)
    sortedHeapSequence == sortedHeapSequence.sorted
  }

  property("hint4") = forAll { (h1: H, h2: H) =>
    val min = findMin(meld(h1, h2))
    (min == findMin(h1)) || (min == findMin(h2))
  }

  property("compareAfterMeld") = forAll { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    val h2Min = findMin(h2)
    findMin(meld(h1, h2)) == math.min(h1Min, h2Min)
  }

  property("meld order") = forAll { (h:H, i:H, j:H) =>
    val h1 = meld(meld(h, i), j)
    val h2 = meld(h, meld(i, j))
    findMin(h1) == findMin(h2)
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    lazy val h2Min = findMin(deleteMin(h2))
    isEmpty(deleteMin(h2)) || findMin(meld(h1,deleteMin(h2))) == math.min(h1Min, h2Min)
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    val h1Min = findMin(h1)
    lazy val h2Min = findMin(deleteMin(h2))
    isEmpty(deleteMin(h2)) || findMin(meld(deleteMin(h2), h1)) == math.min(h1Min, h2Min)
  }

  property("meld3") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(insert(findMin(h1), meld(deleteMin(h1), h2)))
  }

  property("delete1") = forAll { (a: A, b: A, c: A, d: A) =>
    val heap = insert(a, insert(b, insert(c, insert(d, empty))))
    val sortedList = List(a, b, c, d).sorted
    findMin(deleteMin(deleteMin(heap))) == sortedList(2)
  }
}
