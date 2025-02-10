// https://leetcode.com/problems/longest-palindromic-substring/description/
// Longest Palindromic Substring

object Problem005 {

  object Solution {
    import scala.annotation.tailrec

    def longestPalindrome(s: String): String = longestPalindrome(s, "")

    @tailrec
    private def longestPalindrome(in: String, res: String): String =
      if in.isEmpty then res
      else {
        if isPalindrome(in) then if res.length < in.length then in else res
        else {
          var tempString = ""
          var resString = ""
          in.foreach { char =>
            tempString += char
            if (isPalindrome(tempString) && resString.length < tempString.length) {
              resString = tempString
            }
          }
          longestPalindrome(in.tail, if res.length < resString.length then resString else res)
        }
      }

    @tailrec
    private def isPalindrome(in: String, i: Int = 0, res: Boolean = true): Boolean = {
      val j = in.length - i - 1
      if (res && i <= j) {
        isPalindrome(in, i + 1, in.charAt(i) == in.charAt(j))
      } else res
    }
  }

  // Solution By: https://www.youtube.com/watch?v=XYQecbcd6_c
  object Solution1 {
    import scala.annotation.tailrec

    def longestPalindrome(s: String): String =
      if isPalindrome(s) then s
      else {
        var finalResult = ""
        var finalResultLength = 0

        for (i <- s.indices) {
          // odd length
          var l = i
          var r = i
          while (l >= 0 && r < s.length && s.charAt(l) == s.charAt(r)) {
            if ((r - l + 1) > finalResultLength) {
              finalResult = s.substring(l, r + 1)
              finalResultLength = r - l + 1
            }
            l -= 1
            r += 1
          }

          // even length
          l = i
          r = i + 1
          while (l >= 0 && r < s.length && s.charAt(l) == s.charAt(r)) {
            if ((r - l + 1) > finalResultLength) {
              finalResult = s.substring(l, r + 1)
              finalResultLength = r - l + 1
            }
            l -= 1
            r += 1
          }
        }

        finalResult
      }

    @tailrec
    private def isPalindrome(in: String, i: Int = 0, res: Boolean = true): Boolean = {
      val j = in.length - i - 1
      if (res && i <= j) {
        isPalindrome(in, i + 1, in.charAt(i) == in.charAt(j))
      } else res
    }
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.longestPalindrome("babad") == "bab", "test1")
    assert(Solution1.longestPalindrome("babad") == "bab", "test1_1")
    assert(Solution.longestPalindrome("cbbd") == "bb", "test2")
    assert(Solution1.longestPalindrome("cbbd") == "bb", "test2_1")
  }
}
