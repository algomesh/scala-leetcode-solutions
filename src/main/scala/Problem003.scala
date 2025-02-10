// https://leetcode.com/problems/longest-substring-without-repeating-characters/description/
// Longest Substring Without Repeating Characters

object Problem003 {

  object Solution {
    import scala.collection.mutable

    def lengthOfLongestSubstring(s: String): Int = {
      val window = mutable.LinkedHashSet[Char]()
      var tempCount = 0
      var count = 0
      s.foreach { curr =>
        if (window.contains(curr)) {
          count = Math.max(count, tempCount)
          while (curr != window.head) {
            window.remove(window.head)
            tempCount -= 1
          }
          tempCount -= 1
          window.remove(window.head)
        }
        window.add(curr)
        tempCount += 1
      }
      Math.max(count, tempCount)
    }
  }

  def main(args: Array[String]): Unit = {
    import Solution.lengthOfLongestSubstring

    assert(lengthOfLongestSubstring("abcabcbb") == 3, "test1")
    assert(lengthOfLongestSubstring("bbbbb") == 1, "test2")
    assert(lengthOfLongestSubstring("pwwkew") == 3, "test3")
    assert(lengthOfLongestSubstring(" ") == 1, "test4")
  }
}
