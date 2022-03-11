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

  def median2(a: Array[Int], b: Array[Int], a1: Int, a2: Int, b1: Int, b2: Int): Double = {
    if (a1 == a2) {
      medianWithAddedValue(b,b1,b2,a(a1))
    } else if (b1 == b2) {
      medianWithAddedValue(a,a1,a2,b(b1))
    } else {
      val m_a = median1(a,a1,a2)
      val m_b = median1(b,b1,b2)

      val drop = (1+(a2-a1).min(b2-b1))/2
      if (m_a < m_b) {
        median2(a,b,a1+drop,a2,b1,b2-drop)
      } else if (m_a > m_b) {
        median2(a,b,a1,a2-drop,b1+drop,b2)
      } else {
        m_a // m_b
      }
    }
  }

  def medianWithAddedValue(arr: Array[Int], start: Int, end: Int, value: Int) = {
    if (start == end) {
      0.5*(arr(start)+value)
    } else if (start+end % 2 == 0) {
      val mid = (start+end)/2
      if (value < arr(mid-1)) {
        0.5*(arr(mid-1)+arr(mid))
      } else if (value > arr(mid+1)) {
        0.5*(arr(mid)+arr(mid+1))
      } else {
        0.5*(arr(mid)+value)
      } 
    } else {
      val midLeft = (start+end)/2
      val midRight = midLeft+1

      if (value < arr(midLeft)) {
        arr(midLeft)
      } else if (value > arr(midRight)) {
        arr(midRight)
      } else {
        value
      }
    }
  }
}
