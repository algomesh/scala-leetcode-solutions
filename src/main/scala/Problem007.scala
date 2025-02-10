// https://leetcode.com/problems/reverse-integer/description/
// Reverse Integer

object Problem007 {

  object Solution {

    def reverse(x: Int): Int =
      if x >= -9 && x <= 9 then x
      else reverse1(x, 0)

    @scala.annotation.tailrec
    private def reverse(x: Int, out: Long): Int =
      if x == 0 then
        if out < Int.MinValue || out > Int.MaxValue then 0
        else out.intValue
      else reverse(x / 10, (out * 10) + (x % 10))

    @scala.annotation.tailrec
    private def reverse1(x: Int, out: Int): Int =
      if x == 0 then out
      else if out < Int.MinValue / 10 || out > Int.MaxValue / 10 then 0
      else reverse1(x / 10, (out * 10) + (x % 10))
  }

  def main(args: Array[String]): Unit = {
    assert(Solution.reverse(8) == 8, "test1")
    assert(Solution.reverse(0) == 0, "test2")
    assert(Solution.reverse(-9) == -9, "test3")
    assert(Solution.reverse(9) == 9, "test4")

    assert(Solution.reverse(123) == 321, "test4")
    assert(Solution.reverse(-123) == -321, "test5")
    assert(Solution.reverse(120) == 21, "test6")
  }
}
