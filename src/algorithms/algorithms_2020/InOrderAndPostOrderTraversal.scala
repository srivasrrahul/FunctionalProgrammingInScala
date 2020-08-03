

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

//9,3,15,20,7
//9,15,7,20,3
//9,3
//9,3

object Solution {
  def buildTree(inorder: Array[Int], postorder: Array[Int]): TreeNode = {
    def itr(x1 : Int,y1 : Int,x2:Int,y2:Int) : TreeNode = {
      //println(x1 + " " + y1 + "    " + x2 + " " + y2)
      if (x1 == y1) {
        new TreeNode(inorder(x1))
      }else {
        //root is last
        val rootValue = postorder(y2)
        var rootInOrderIndex = x1
        for (j <- x1 to y1) {
          if (inorder(j) == rootValue) {
            rootInOrderIndex = j
          }
        }

        val diff = rootInOrderIndex-x1
        //left to root and right to root
        var leftTreeNode : TreeNode = null
        if (diff >= 1) {
          leftTreeNode = itr(x1,rootInOrderIndex-1,x2,x2+diff-1)
        }

        var rightTreeNode :TreeNode = null
        if (rootInOrderIndex+1 <= y1) {
          rightTreeNode = itr(rootInOrderIndex+1,y1,x2+diff,y2-1)
        }

        new TreeNode(rootValue,leftTreeNode,rightTreeNode)
      }
    }

    if (inorder.isEmpty) {
      null
    }else {
      itr(0,inorder.length-1,0,postorder.length-1)
    }

  }
}