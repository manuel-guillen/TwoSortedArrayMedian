import scala.annotation.tailrec
import scala.math.Integral.Implicits._

object Main {
  def findMedianSortedArrays(a: Array[Int], b: Array[Int]) = {
    (a.length, b.length) match {
      case (0,n) => median1(b,0,n-1)
      case (m,0) => median1(a,0,m-1)
      case (m,n) => median2(a,b,0,m-1,0,n-1)
    }
  }

  def median1(arr: Array[Int], start: Int, end: Int) = {
    val (mid,r) = (start+end) /% 2
    if (r == 0) arr(mid).toDouble else 0.5*(arr(mid)+arr(mid+1))
  }

  @tailrec
  def median2(a: Array[Int], b: Array[Int], a1: Int, a2: Int, b1: Int, b2: Int): Double = {
    (a2-a1+1,b2-b1+1) match {
      case (1,1) => 0.5*(a(a1)+b(b1))
      case (_,1) => medianWithAddedValue(a,a1,a2,b(b1))
      case (1,_) => medianWithAddedValue(b,b1,b2,a(a1))
      case (2,2) => val arr = Array(a(a1),a(a2),b(b1),b(b2)).sorted; 0.5*(arr(1)+arr(2))
      case (_,2) => medianWithTwoAddedValues(a,a1,a2,b(b1),b(b2))
      case (2,_) => medianWithTwoAddedValues(b,b1,b2,a(a1),a(a2))
      case (m,n) =>
        val m_a = median1(a,a1,a2)
        val m_b = median1(b,b1,b2)
        val drop = ((m min n)-1)/2
        if (m_a < m_b)
          median2(a,b,a1+drop,a2,b1,b2-drop)
        else if (m_a > m_b)
          median2(a,b,a1,a2-drop,b1+drop,b2)
        else
          m_a // m_b
    }
  }

  def medianWithAddedValue(arr: Array[Int], start: Int, end: Int, value: Int) = {
    if ((start+end) % 2 == 0) {
      val mid = (start+end)/2

      if (value <= arr(mid-1))       0.5*(arr(mid-1)+arr(mid))
      else if (value >= arr(mid+1))  0.5*(arr(mid)+arr(mid+1))
      else                           0.5*(arr(mid)+value)
    }
    else {
      val midLeft = (start+end)/2
      val midRight = midLeft+1

      if (value <= arr(midLeft))        arr(midLeft)
      else if (value >= arr(midRight))  arr(midRight)
      else                              value
    }
  }

  def medianWithTwoAddedValues(arr: Array[Int], start: Int, end: Int, val1: Int, val2: Int): Double = {
    if ((start+end) % 2 == 0) {
      val mid = (start+end)/2

      if (val1 <= arr(mid-1) && val2 <= arr(mid-1))       arr(mid-1)
      else if (val1 >= arr(mid+1) && val2 >= arr(mid+1))  arr(mid+1)
      else if (val1 <= arr(mid) && val2 >= arr(mid))      arr(mid)
      else if (val2 <= arr(mid))                          val2
      else if (val1 >= arr(mid))                          val1
      else                                                throw new RuntimeException("Reached expected-dead code. Should not be reached.")
    }
    else {
      val midLeft = (start+end)/2
      val midRight = midLeft+1

      if (val1 <= arr(midLeft-1) && val2 <= arr(midLeft-1))         0.5*(arr(midLeft-1)+arr(midLeft))
      else if (val1 >= arr(midRight+1) && val2 >= arr(midRight+1))  0.5*(arr(midRight)+arr(midRight+1))
      else if (val1 <= arr(midLeft) && val2 >= arr(midRight))       0.5*(arr(midLeft)+arr(midRight))
      else if (val2 <= arr(midLeft))                                0.5*(val2+arr(midLeft))
      else if (val1 >= arr(midRight))                               0.5*(arr(midRight)+val1)
      else if (val1 <= arr(midLeft) && arr(midLeft) <= val2)        0.5*(arr(midLeft)+val2)
      else if (val1 <= arr(midRight) && arr(midRight) <= val2)      0.5*(val1+arr(midRight))
      else                                                          0.5*(val1+val2)
    }
  }
}
