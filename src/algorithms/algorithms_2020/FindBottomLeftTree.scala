import scala.collection.mutable

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def findBottomLeftValue(root: TreeNode): Int = {

    val q = new mutable.Queue[TreeNode]()

    if (root != null) {
      q.addOne(root)
    }

    var solution = root
    while (q.isEmpty == false) {
      solution = q.dequeue()
      val tops = q.dequeueAll(_ => true)
      if (solution.left != null) {
        q.addOne(solution.left)
      }

      if (solution.right != null) {
        q.addOne(solution.right)
      }

      for (top <- tops) {
        if (top.left != null) {
          q.addOne(top.left)
        }

        if (top.right != null) {
          q.addOne(top.right)
        }
      }


    }

    solution.value
  }
}