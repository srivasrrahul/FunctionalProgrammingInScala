import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def postorderTraversal(root: TreeNode): List[Int] = {
    val stack = new mutable.Stack[(TreeNode,Int)]() //INt ==> 0 then 1 if 1 then dont insert
    val lstBuffer = new ListBuffer[Int]

    if (root != null) {
      stack.push((root,0)) //0 left is unfinished, 1 left is added but unfinihsed, 2 left is finished and added right
    }

    var current = root
    while (stack.isEmpty == false) {
      val (top,state) = stack.pop()
      state match {
        case 0 => {
          val left = top.left
          if (left != null) {
            stack.push((top,1))
            stack.push((left,0))
          }else {
            //if left is null
            val right = top.right
            if (right != null) {
              stack.push((top,2))
              stack.push((right,0))
            }else {
              //its leaft
              lstBuffer.append(top.value)
            }
          }
        }
        case 1 => {
          //left is proessed
          val right = top.right
          if (right != null) {
            stack.push((top,2))
            stack.push((right,0))
          }else {
            lstBuffer.append(top.value)
          }
        }
        case 2 => {
          lstBuffer.append(top.value)
        }
      }


    }

    lstBuffer.toList
  }
}