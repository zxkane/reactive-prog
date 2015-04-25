package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import scala.util._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h))==m
  }
  
  property("insert 2 items") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    Math.min(a, b) == findMin(h)
    val h2 = deleteMin(h)
    Math.max(a, b) == findMin(h2)
  }
  
  property("merge empty heaps returns empty") = forAll { (h: H) =>
    isEmpty(meld(empty, empty))
  }
  
  property("merge empty heaps returns self") = forAll { (h: H) =>
    val meldH = meld(empty, h)
    meldH == h
  }
   
  property("merge heaps") = forAll { (h: H, h2: H) =>
    val meldH = meld(h2, h)
    findMin(meldH) == Math.min(findMin(h), findMin(h2))
  }
  
  property("delete min") = forAll { (a: Int, b: Int, c: Int) =>
    val h = insert(a, insert(b, insert(c, empty)))
    deleteMin(deleteMin(h)) == insert(Math.max(a, Math.max(b, c)), empty)
  }  

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(n, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
