// https://leetcode.com/problems/add-two-numbers/description/
// Add Two Numbers

object Problem002 {

  private object Solution {
    import scala.annotation.tailrec

    def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = sum(l1, l2, 0, null, null)

    @tailrec
    private def sum(l1: ListNode, l2: ListNode, r: Int, firstNode: ListNode, lastNode: ListNode): ListNode =
      if (l1 != null || l2 != null || r != 0) {
        val a = if l1 == null then 0 else l1.x
        val b = if l2 == null then 0 else l2.x
        val c = a + b + r
        val node = ListNode(c % 10)

        if (lastNode != null) {
          lastNode.next = node
        }

        sum(
          if l1 == null then l1 else l1.next,
          if l2 == null then l2 else l2.next,
          c / 10,
          if firstNode == null then node else firstNode,
          if lastNode == null then node else lastNode.next
        )
      } else firstNode
  }

  def main(args: Array[String]): Unit = {
    import Solution.addTwoNumbers

    val l1 = ListNode(2, ListNode(4, ListNode(3)))
    val l2 = ListNode(5, ListNode(6, ListNode(4)))
    val l3 = ListNode(7, ListNode(0, ListNode(8)))
    assert(iterator(addTwoNumbers(l1, l2)).sameElements(iterator(l3)), "test1")

    val l4 = ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9, ListNode(9)))))))
    val l5 = ListNode(9, ListNode(9, ListNode(9, ListNode(9))))
    val l6 = ListNode(8, ListNode(9, ListNode(9, ListNode(9, ListNode(0, ListNode(0, ListNode(0, ListNode(1))))))))
    assert(iterator(addTwoNumbers(l4, l5)).sameElements(iterator(l6)), "test2")

    val l7 = ListNode(2, ListNode(4, ListNode(9)))
    val l8 = ListNode(5, ListNode(6, ListNode(4, ListNode(9))))
    val l9 = ListNode(7, ListNode(0, ListNode(4, ListNode(0, ListNode(1)))))
    assert(iterator(addTwoNumbers(l7, l8)).sameElements(iterator(l9)), "test3")
  }

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  private def iterator(node: ListNode) =
    new Iterator[Int] {
      private var _next = node
      override def next(): Int = {
        val res = _next
        _next = _next.next
        res.x
      }
      override def hasNext: Boolean = _next != null
    }
}
