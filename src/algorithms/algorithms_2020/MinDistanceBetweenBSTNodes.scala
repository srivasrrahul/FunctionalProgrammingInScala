import scala.collection.mutable.ListBuffer

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}
object Solution {
  type MinSum = Int
  type SmallestVal = Int
  type LargestVal = Int
  def minDiffInBST(root: TreeNode): Int = {
    def getAll(current : TreeNode) : ListBuffer[Int] = {
      if (current == null) {
        new ListBuffer[Int]
      }else {
        val leftLst = getAll(current.left)
        val rightLst = getAll(current.right)
        leftLst.addOne(current.value)
        leftLst.addAll(rightLst)

        leftLst
      }

    }

    val lst = getAll(root).toList
    var minDiff = Int.MaxValue
    var current = lst.head

    lst.tail.foreach(y => {
      val diff = y - current
      if (diff < minDiff) {
        minDiff = diff
      }

      current = y
    })

    minDiff

  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(2)
    val left = new TreeNode(1)
    val right = new TreeNode(2)
    root.left = left
    root.right = right
    println(minDiffInBST(root))
  }
}