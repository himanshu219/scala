package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (9, genHeap))
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  property("The smallest of 2 elements should be the smallest in a previously empty Heap") = forAll { (h: H) =>
    if (isEmpty(h)) {
      var nh = insert(4, h)
      nh = insert(5, nh)
      findMin(nh) == 4
    } else {
      true
    }
  }
  property("Deleting the minimal value in a Heap should result in an empty Heap") = forAll { (h: H) =>
    if (isEmpty(h)) {
      var nh = insert(4, h)
      nh = deleteMin(nh)
      isEmpty(nh)
    } else {
      true
    }
  }

  property("he minimal value of a melded Heap should be the min of the min of both heaps") = forAll { (h1: H, h2: H) => {

    def fmin(h: H): Int =
      if (isEmpty(h)) Int.MinValue else findMin(h)

    val nh = meld(h1, h2)
    fmin(nh) == fmin(h1) || fmin(nh) == fmin(h2)
    }
  }

  property("Recusivly finding and deleting elements in a Heap should return sorted elements") = forAll { (h: H) =>
    def issorted(nh: H, par: Int): Boolean = {
      if (isEmpty(nh)) true
      else {
        val cm = findMin(nh)
        val adh = deleteMin(nh)
        par <= cm && issorted(adh, cm)
      }
    }
    issorted(h, Int.MinValue)
  }

  property("meldsorted") = forAll { (h2: H, h1: H) =>
    def issorted(nh: H, par: Int): Boolean = {
      if (isEmpty(nh)) true
      else {
        val cm = findMin(nh)
        val adh = deleteMin(nh)
        par <= cm && issorted(adh, cm)
      }
    }
    issorted(meld(h1, h2), Int.MinValue)
  }

  property("insert min after delete results in original") = forAll { (h: H) =>
    def sortheap(nh: H): List[Int] = {
      if (isEmpty(nh)) Nil
      else {
        val cm = findMin(nh)
        val adh = deleteMin(nh)
        cm :: sortheap(adh)
      }
    }
    if (!isEmpty(h)) {
      val mh = findMin(h)
      val dh = deleteMin(h)
      val nh = insert(mh, dh)
      sortheap(h) == sortheap(nh)
    } else {
      true
    }
  }

  property("The minimal value of 2 heaps should be the minimal after dispacing it from heap 1 to 2 and melding both") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      val m = m1 min m2
      val mh = meld(deleteMin(h1), insert(m, h2))
      !isEmpty(mh) && findMin(mh) == m
    }
  }



  property("The minimal value of a melded Heap should be the min of the min of both heaps") = forAll { (h1: H, h2: H) =>
    findMin(meld(h1, h2)) == findMin(h1).min(findMin(h2))
  }


  property("Two heaps should be equal if recursivly removing min elements result in same elements until empty") = forAll { (h1: H, h2: H) =>
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

  property("T he minimal value of 2 heaps should be the minimal after dispacing it from heap 1 to 2 and melding both") = forAll { (h1: H, h2: H) =>
    if (isEmpty(h1) || isEmpty(h2)) true
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = m1 min m2
    val mh = meld(deleteMin(h1), insert(m, h2))

    !isEmpty(mh) && findMin(mh) == m
  }

}
