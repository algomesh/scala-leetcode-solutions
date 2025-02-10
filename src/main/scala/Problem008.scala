// https://leetcode.com/problems/string-to-integer-atoi/description/
// String to Integer (atoi)

object Problem008 {

  object Solution {

    private val (startAscii, endAscii) = (48, 57)

    def myAtoi(s: String): Int = myAtoi(s, 0, None, None)

    @scala.annotation.tailrec
    private def myAtoi(s: String, i: Int, out: Option[Long], isPositive: Option[Boolean]): Int =
      if out.exists(_ > Int.MaxValue) then Int.MaxValue
      else if out.exists(_ < Int.MinValue) then Int.MinValue
      else if i >= s.length then out.map(_.intValue).getOrElse(0)
      else if s.charAt(i) == ' ' && out.isEmpty then myAtoi(s, i + 1, out, isPositive)
      else if s.charAt(i) == '+' && out.isEmpty && isPositive.isEmpty then myAtoi(s, i + 1, Some(0), Some(true))
      else if s.charAt(i) == '-' && out.isEmpty && isPositive.isEmpty then myAtoi(s, i + 1, Some(0), Some(false))
      else if s.charAt(i) < startAscii || s.charAt(i) > endAscii then out.map(_.intValue).getOrElse(0)
      else {
        val curr: Long = Math.abs(out.getOrElse(0L))
        val next: Long = (curr * 10) + (s.charAt(i) - startAscii)
        val withSign = if !isPositive.getOrElse(true) then next * -1 else next
        myAtoi(s, i + 1, Some(withSign), isPositive)
      }
  }

  def main(args: Array[String]): Unit = {
    import Solution.myAtoi

    assert(myAtoi("") == 0, "test1")
    assert(myAtoi("           ") == 0, "test2")
    assert(myAtoi("    123") == 123, "test3")
    assert(myAtoi("    123 ") == 123, "test4")
    assert(myAtoi("    123  456") == 123, "test5")
    assert(myAtoi("    123C456") == 123, "test6")
    assert(myAtoi("    -123C456") == -123, "test7")
    assert(myAtoi("    -000000123C456") == -123, "test8")
    assert(myAtoi("21474836460") == Int.MaxValue, "test9")

    assert(myAtoi("42") == 42, "test10")
    assert(myAtoi(" -042") == -42, "test11")
    assert(myAtoi("1337c0d3") == 1337, "test12")
    assert(myAtoi("0-1") == 0, "test13")
    assert(myAtoi("words and 987") == 0, "test14")

  }
}
