class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def constructMaximumBinaryTree(nums: Array[Int]): TreeNode = {
    def constructNode(begin : Int,end : Int) : TreeNode = {
      if (begin == end) {
         new TreeNode(nums(begin))
      }else {
        if (begin > end) {
          null
        } else {
          //find max between begin and end
          var maxItem = nums(begin)
          var maxIndex = begin

          for (j <- begin + 1 to end) {
            if (nums(j) > maxItem) {
              maxItem = nums(j)
              maxIndex = j
            }
          }

          val leftTree = constructNode(begin, maxIndex - 1)
          val rightTree = constructNode(maxIndex + 1, end)
          new TreeNode(maxItem, leftTree, rightTree)
        }
      }
    }

    constructNode(0,nums.length-1)
  }

  def main(args: Array[String]): Unit = {

  }
}