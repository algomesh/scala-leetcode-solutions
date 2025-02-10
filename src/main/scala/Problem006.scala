// https://leetcode.com/problems/zigzag-conversion/description/
// Zigzag Conversion

object Problem006 {

  object Solution {
    import scala.collection.mutable

    def convert(s: String, numRows: Int): String =
      if s.length <= 1 || numRows <= 1 || s.length <= numRows then s
      else if numRows == 2 then computeFor2Rows(s)
      else computeForNRows(s, numRows)

    private def computeFor2Rows(s: String) = {
      val array: Array[Char] = Array.ofDim(s.length)
      var evenInd = 0
      var oddInd = if s.length % 2 == 0 then s.length / 2 else s.length / 2 + 1
      array(evenInd) = s.charAt(0)
      array(oddInd) = s.charAt(1)

      for (i <- 2 until s.length)
        if (i % 2 == 0) {
          evenInd += 1
          array(evenInd) = s.charAt(i)
        } else {
          oddInd += 1
          array(oddInd) = s.charAt(i)
        }

      new String(array)
    }

    private def computeForNRows(s: String, n: Int) = {
      val arr: Array[mutable.ArrayBuffer[Char]] = Array.ofDim(n)
      val range = Solution.Range(0, n)

      for (i <- s.indices) {
        val idx = range.getAndUpdate
        val inArr = Option(arr(idx)).getOrElse(mutable.ArrayBuffer())
        inArr.append(s.charAt(i))
        arr(idx) = inArr
      }

      arr.map(f => new String(f.toArray)).mkString
    }

    private class Range(start: Int, end: Int) {
      private var isIncreasing = start < end
      private var i: Int = if isIncreasing then start else end

      def getAndUpdate: Int = {
        val r = i

        if (isIncreasing && r < end - 1) {
          i += 1
        } else if (r == end - 1) {
          isIncreasing = false
          i -= 1
        } else if (!isIncreasing && r > start) {
          i -= 1
        } else if (r == start) {
          isIncreasing = true
          i += 1
        }

        r
      }
    }
  }

  // https://www.youtube.com/watch?v=Q2Tw6gcVEwc
  object Solution1 {
    def convert(s: String, numRows: Int): String =
      if s.length <= 1 || numRows <= 1 || s.length <= numRows then s
      else {
        var res = ""

        for (i <- 0 until numRows) {
          val increment = 2 * (numRows - 1)
          for (j <- i until s.length by increment) {
            res += s.charAt(j)
            val midIndex = j + increment - 2 * i
            if (i > 0 && i < numRows - 1 && midIndex < s.length) {
              res += s.charAt(midIndex)
            }
          }
        }
        res
      }
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.convert("ABCD", 1) == "ABCD", "test1")
    assert(Solution1.convert("ABCD", 1) == "ABCD", "test1_1")

    assert(Solution.convert("A", 2) == "A", "test2")
    assert(Solution1.convert("A", 2) == "A", "test2_1")
    assert(Solution.convert("AB", 2) == "AB", "test3")
    assert(Solution1.convert("AB", 2) == "AB", "test3_1")
    assert(Solution.convert("ABC", 2) == "ACB", "test4")
    assert(Solution1.convert("ABC", 2) == "ACB", "test4_1")
    assert(Solution.convert("ABCD", 2) == "ACBD", "test5")
    assert(Solution1.convert("ABCD", 2) == "ACBD", "test5_1")
    assert(Solution.convert("ABCDE", 2) == "ACEBD", "test7")
    assert(Solution1.convert("ABCDE", 2) == "ACEBD", "test7_1")
    assert(Solution.convert("ABCDEF", 2) == "ACEBDF", "test8")
    assert(Solution1.convert("ABCDEF", 2) == "ACEBDF", "test8_1")

    assert(Solution.convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR", "test9")
    assert(Solution.convert("PAYPALISHIRING", 3) == "PAHNAPLSIIGYIR", "test9_1")
    assert(Solution.convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI", "test10")
    assert(Solution.convert("PAYPALISHIRING", 4) == "PINALSIGYAHRPI", "test10_1")
  }
}
