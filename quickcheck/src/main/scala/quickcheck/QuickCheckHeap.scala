package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = for {
    length <- Gen.choose(0, 100)
    values <- Gen.listOfN(length, Arbitrary.arbitrary[Int])
  } yield values.foldLeft(empty)((heap, value) => insert(value, heap))

  lazy val genHeapV2: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(a, h)

  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("insert and min") = forAll { (a: A) =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("meld min element") = forAll { (h1: H, h2: H) =>
    val minElement1 = findMin(h1)
    val minElement2 = findMin(h2)
    val meldedHeap = meld(h1, h2)
    val meldedMinElement = findMin(meldedHeap)

    val ans = meldedMinElement == minElement1 || meldedMinElement == minElement2
    if !ans then println(meldedMinElement)
    ans
  }

  property("deleteMin") = forAll { (h: H) =>
    if isEmpty(h) then
      true
    else
      val m = if isEmpty(h) then 0 else findMin(h)
      val h2 = deleteMin(h)
      if isEmpty(h2) then true else findMin(h2) >= m
  }

  property("order of elements") = forAll { (h: H) =>
    def isSorted(h: H): Boolean = {
      if (isEmpty(h)) true
      else {
        val m = findMin(h)
        val h2 = deleteMin(h)
        isEmpty(h2) || (m <= findMin(h2) && isSorted(h2))
      }
    }

    isSorted(h)
  }
