// https://leetcode.com/problems/palindrome-number/description/
//Palindrome Number

object Problem009 {

  object Solution {
    def isPalindrome(x: Int): Boolean =
      if x < 0 then false
      else if x >= 0 && x <= 9 then true
      else isPalindrome(x.toString, 0, true)

    @scala.annotation.tailrec
    private def isPalindrome(s: String, i: Int, out: Boolean): Boolean = {
      val j = s.length - i - 1
      if i > j || !out then out
      else isPalindrome(s, i + 1, s.charAt(i) == s.charAt(j))
    }
  }

  def main(args: Array[String]): Unit = {
    import Solution.isPalindrome

//    assert(isPalindrome(121), "test1")
//    assert(!isPalindrome(-121), "test2")
    assert(!isPalindrome(10), "test3")
  }
}
