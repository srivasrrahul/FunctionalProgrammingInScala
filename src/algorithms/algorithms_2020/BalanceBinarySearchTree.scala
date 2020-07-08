class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def balanceBST(root: TreeNode): TreeNode = {
    def createTree(lst : List[Int]) : (TreeNode,Int) = {
      if (lst.isEmpty) {
        (null,0)
      }else {
        if (lst.size == 1) {
          (new TreeNode(lst.head),1)
        }else {
          //println("Creatinh tree " + lst)
          val (firstHalf,secondHalf) = lst.splitAt(lst.length/2)

          val (leftNode,leftHeight) = createTree(firstHalf)
          val topNode = new TreeNode(secondHalf.head)
          val (rightNode,rightHeight) = createTree(secondHalf.tail)

          topNode.left = leftNode
          topNode.right = rightNode
          (topNode,1+math.max(leftHeight,rightHeight))
        }
      }

    }
    def balance(node : TreeNode) : (List[Int],(TreeNode,Int)) = {
      if (node == null) {
        (List(),(null,0))
      }else {
        val (leftLst,(leftNode,leftSize)) = balance(node.left)
        val (rightLst,(rightNode,rightSize)) = balance(node.right)
        node.left = leftNode
        node.right = rightNode

        if (math.abs(leftSize-rightSize) <= 1) {
          //println("ss")
          ((leftLst ++ List(node.value) ++ rightLst),(node,math.max(leftSize,rightSize)+1))
        }else {
          //println("hello")
          //balance it by creating a new treeNode and splitting to half
          val combinedLst = leftLst ++ List(node.value) ++ rightLst
          val (newNode,newHeight) = createTree(combinedLst)
          (combinedLst,(newNode,newHeight))
        }
      }
    }

    val (_,(newRoot,_)) = balance(root)
    newRoot

  }
}