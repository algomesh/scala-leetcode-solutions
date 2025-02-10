// https://leetcode.com/problems/two-sum/description/
// Two Sum

object Problem001 {

  object Solution {
    import scala.annotation.tailrec
    import scala.collection.mutable

    def twoSum(nums: Array[Int], target: Int): Array[Int] =
      twoSum(nums, target, 0, mutable.HashMap())

    @tailrec
    private def twoSum(nums: Array[Int], target: Int, index: Int, acc: mutable.Map[Int, Int]): Array[Int] =
      if (index < nums.length) {
        val diff = target - nums(index)
        acc.get(diff) match {
          case Some(prevIndex) => Array(prevIndex, index)
          case None =>
            acc.update(nums(index), index)
            twoSum(nums, target, index + 1, acc)
        }
      } else Array.empty
  }

  def main(args: Array[String]): Unit = {
    import Solution.twoSum

    assert(twoSum(Array(2, 7, 11, 15), 9).sameElements(Array(0, 1)), "test1")
    assert(twoSum(Array(3, 2, 4), 6).sameElements(Array(1, 2)), "test2")
    assert(twoSum(Array(3, 3), 6).sameElements(Array(0, 1)), "test3")
  }
}
