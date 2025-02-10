// https://leetcode.com/problems/median-of-two-sorted-arrays/description/
// Median of Two Sorted Arrays

object Problem004 {

  object Solution {
    import scala.annotation.tailrec

    def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = {
      val length1 = nums1.length
      val length2 = nums2.length
      val totalLength = length1 + length2

      if totalLength % 2 == 0 then medianEven(nums1, nums2, (totalLength / 2) - 1)
      else medianOdd(nums1, nums2, ((totalLength + 1) / 2) - 1)
    }

    @tailrec
    private def medianEven(
      num1: Array[Int],
      num2: Array[Int],
      medianIndex: Int,
      loopVariable: Int = 0,
      i: Int = 0,
      j: Int = 0
    ): Double = {
      val a = getFromArray(num1, i)
      val b = getFromArray(num2, j)

      if loopVariable == medianIndex then {
        if (a < b) {
          val r1: Double = a
          val r2: Double = Math.min(getFromArray(num1, i + 1), b)
          (r1 + r2) / 2
        } else if (b < a) {
          val r1: Double = b
          val r2: Double = Math.min(getFromArray(num2, j + 1), a)
          (r1 + r2) / 2
        } else {
          val r1: Double = a
          val r2: Double = b
          (r1 + r2) / 2
        }
      } else {
        val (newI, newJ) = if a <= b then (i + 1, j) else (i, j + 1)
        medianEven(num1, num2, medianIndex, loopVariable + 1, newI, newJ)
      }
    }

    @tailrec
    private def medianOdd(
      num1: Array[Int],
      num2: Array[Int],
      medianIndex: Int,
      loopVariable: Int = 0,
      i: Int = 0,
      j: Int = 0
    ): Double = {
      val a = getFromArray(num1, i)
      val b = getFromArray(num2, j)

      if loopVariable == medianIndex then Math.min(a, b)
      else {
        val (newI, newJ) = if a <= b then (i + 1, j) else (i, j + 1)
        medianOdd(num1, num2, medianIndex, loopVariable + 1, newI, newJ)
      }
    }

    private def getFromArray(arr: Array[Int], index: Int) =
      if index < arr.length then arr(index) else Int.MaxValue
  }

  def main(args: Array[String]): Unit = {
    import Solution.findMedianSortedArrays

    // odd lengths
    assert(findMedianSortedArrays(Array(1, 3), Array(2)) == 2, "test1")
    assert(findMedianSortedArrays(Array(1, 2), Array(3)) == 2, "test2")
    assert(findMedianSortedArrays(Array(1, 7), Array(2, 4, 5, 6, 9)) == 5, "test3")
    assert(findMedianSortedArrays(Array(2, 4, 6, 9), Array(1, 5, 7)) == 5, "test4")
    assert(findMedianSortedArrays(Array(2, 2, 2, 2, 4, 6, 6, 9), Array(1, 5, 7)) == 4, "test5")
    assert(findMedianSortedArrays(Array(2, 2, 2, 2, 4, 6, 9), Array(1, 1, 1, 5, 5, 7)) == 2, "test6")
    assert(findMedianSortedArrays(Array(1), Array.empty) == 1, "test7")
    assert(findMedianSortedArrays(Array.empty, Array(1)) == 1, "test8")

    // even lengths
    assert(findMedianSortedArrays(Array(1, 2), Array(3, 4)) == 2.5, "test9")
    assert(findMedianSortedArrays(Array(3, 4), Array(1, 2)) == 2.5, "test10")
    assert(findMedianSortedArrays(Array(3, 3, 4), Array(1, 2, 2)) == 2.5, "test11")
  }
}
