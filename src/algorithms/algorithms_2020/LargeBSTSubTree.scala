

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}


case class Result(val isBST : Boolean,val subTreeSize : Int,val max : Int,val min : Int)
object Solution {
  def largestBSTSubtree(root: TreeNode): Int = {
    var maxBSTSize = 0
    def check(node : TreeNode) : Result = {
      if (node == null) {
        new Result(true, 0,-1,-1)
      }else {
        val leftResult = check(node.left)
        val rightResult = check(node.right)

        if (leftResult.isBST && rightResult.isBST) {
          (leftResult.subTreeSize,rightResult.subTreeSize) match {
            case (0,0) => {
              val result = new Result(true,1,node.value,node.value)
              if (maxBSTSize == 0) {
                maxBSTSize = 1
              }

              result
            }
            case (_,0) => {
              if (leftResult.max < node.value) {
                //println("Here " + node.value + " " + leftResult.max)
                val result = new Result(true,leftResult.subTreeSize+1,node.value,leftResult.min)
                if (maxBSTSize < leftResult.subTreeSize+1) {
                  maxBSTSize = leftResult.subTreeSize+1
                }

                result
              }else {
                new Result(false,0,0,0)
              }
            }
            case (0,_) => {
              if (rightResult.min > node.value) {
                val result = new Result(true,rightResult.subTreeSize+1,rightResult.max,node.value)
                if (maxBSTSize < rightResult.subTreeSize+1) {
                  maxBSTSize = rightResult.subTreeSize+1
                }

                result
              }else {
                new Result(false,0,0,0)
              }
            }
            case (_,_) => {
              if (leftResult.max < node.value && rightResult.min > node.value) {
                val result = new Result(true,leftResult.subTreeSize+rightResult.subTreeSize+1,
                  rightResult.max,leftResult.min)

                if (maxBSTSize < leftResult.subTreeSize+rightResult.subTreeSize+1) {
                  maxBSTSize = leftResult.subTreeSize+rightResult.subTreeSize+1
                }

                result
              }else {
                new Result(false,0,0,0)
              }
            }
          }
        }else {
          new Result(false,0,0,0)
        }
      }
    }

    check(root)
    maxBSTSize
  }
}