package quickcheck

import common._
import org.scalacheck._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    elem <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    h <- oneOf(const(empty), genHeap)
  } yield insert(elem, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def genList(h: H): List[Int] = if (isEmpty(h)) Nil else findMin(h) :: genList(deleteMin(h))

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMin1") = forAll { (a: Int, b: Int) =>
    val h1 = insert(a, empty)
    val h2 = insert(b, h1)
    findMin(h2) == ord.min(a, b)
  }

  property("delete1") = forAll { a: Int =>
    val h1 = insert(a, empty)
    val h2 = deleteMin(h1)
    h2 == empty
  }

  property("meld1") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    findMin(meld(h1, h2)) == ord.min(min1, min2)
  }

  property("delete2") = forAll { h: H =>
    val list = genList(h)
    list == list.sorted
  }

  property("link1") = forAll { (l: List[A]) =>
    val h = l.foldRight(empty)(insert)
    genList(h) == l.sorted
  }

}
