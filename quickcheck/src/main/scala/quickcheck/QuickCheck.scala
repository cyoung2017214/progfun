package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = {
    for {
      i <- arbitrary[Int]
      h <- oneOf[H]( empty, genHeap )
    } yield insert(i, h)
  }



  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /*
  If you insert any two elements into an empty heap, finding the minimum of the resulting heap should get the smallest of the two elements back.
  */

  property("insert2elementsMin") = forAll { (a: Int, b: Int ) =>
    val h = insert(a, empty)
    val h2 = insert(b, h)

    findMin(h2) == Math.min(a,b)
  }
  /*
If you insert an element into an empty heap, then delete the minimum, the resulting heap should be empty.*/
  property("insertAnElementAndThenDeleteTheMinimum") = forAll { (a: Int)=>
    val h = insert(a,empty)
    isEmpty( deleteMin(h) )
  }

  def findAndDeleteMin(h : H) : List[Int] = {
    if ( isEmpty(h) ) Nil
    else findMin(h) :: findAndDeleteMin(deleteMin(h) )

  }
  /*
Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima. (Hint: recursion and helper functions are your friends.)*/

  property("sortedSequence") = forAll { (h: H) =>
    /*val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m*/
    val result = findAndDeleteMin(h)
    (result, result.tail).zipped.forall(_ <= _)

  }
  /*
Finding a minimum of the melding of any two heaps should return a minimum of one or the other.

   */

  property("meldAndFinMin") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val m = findMin(h)
    val l = findAndDeleteMin(h)
    val l1 = findAndDeleteMin(h1)
    val l2 = findAndDeleteMin(h2)

    (m == Math.min(findMin(h1), findMin(h2)) & ((( l1 ::: l2 ).sorted).zip(l).filter( x=> x._1 != x._2).size == 0 ))
  }

  /* from couse web site discussion
  *
  * Take 2 integers, put them in empty heap, and try to get them back. First you'll get the minimum of those two values,
  * and then you'll get their maximum.
    Put at least 3 different integers in empty heap, and findMin will be the smallest one of those integers.
    If you deleteMin, findMin should return bigger value.*/
/*
  property("3integers") = forAll { ( a : Int ) =>

    val h = insert(a+2, (insert(a+1, insert(a, empty))))
    val h1 = deleteMin(h)
    val h2 = deleteMin(h1)
    val h3 = deleteMin(h2)
    ( (a == findMin(h)) & ( (a+1) == findMin(h1) ) & ( (a+2) == findMin(h2) ) & ( isEmpty(h3) ) )
  }

  property("melding 3 times and deleting 3 mins, next min are equal") =
    forAll { (h: H) =>
      val hm = meld(meld(h, h), h)
      val h1 = deleteMin(deleteMin(deleteMin(hm)))
      val h2 = deleteMin(h)
      isEmpty(h2) || findMin(h1) == findMin(h2)
    }*/
}
