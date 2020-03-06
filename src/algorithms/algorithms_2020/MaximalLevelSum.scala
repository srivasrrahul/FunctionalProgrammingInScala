import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(var _value: Int) {
 var value: Int = _value
 var left: TreeNode = null
 var right: TreeNode = null
 }

object Solution {
  def maxLevelSum(root: TreeNode): Int = {
    var current_val_level = 1
    var max_sum = root.value
    var max_sum_level = 1

    val next = new ListBuffer[TreeNode]()
    if (root.left != null) {
      next.addOne(root.left)
    }

    if (root.right != null) {
      next.addOne(root.right)
    }

    while (next.isEmpty == false) {
      current_val_level = current_val_level + 1
      //Extract all
      var local_level_sum = 0
      val next_level = new ListBuffer[TreeNode]

      next.foreach {
        node =>
          local_level_sum += node.value
          if (node.left != null) {
            next_level.addOne(node.left)
          }
          if (node.right != null) {
            next_level.addOne(node.right)
          }
      }

      if (local_level_sum > max_sum) {
        max_sum = local_level_sum
        max_sum_level = current_val_level
      }

      next.clear()
      next.addAll(next_level)
    }

    max_sum_level
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    val l1_1 = new TreeNode(7)
    val l1_2 = new TreeNode(0)
    val l2_1 = new TreeNode(7)
    val l2_2 = new TreeNode(-8)

    root.left = l1_1
    root.right = l1_2

    l1_1.left = l2_1
    l1_1.right = l2_2

    println(maxLevelSum(root))
  }
}