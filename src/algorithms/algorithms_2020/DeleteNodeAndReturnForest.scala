import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right

  override def toString: String = {
    value.toString
  }
}

object Solution {
  def delNodes(root: TreeNode, to_delete: Array[Int]): List[TreeNode] = {
    val deletionSet = to_delete.toSet
    def collect(curretNode : TreeNode) : (List[TreeNode],TreeNode)  = {
      if (curretNode == null) {
        (List(),null)
      }else {
        if (curretNode.left == null && curretNode.right == null) {
          //Leaf and check if value exist in deletion set
          if (deletionSet.contains(curretNode.value)) {
            (List(),null)
          }else {
            //It's connected by top
            (List(),curretNode)
          }
        }else {
          val (leftForest,leftChildConnectedNode) = collect(curretNode.left)
          val (rightForest,rightChildConnectedNode) = collect(curretNode.right)

          if (deletionSet.contains(curretNode.value)) {
            //It creates a forest
            val newForest = new ListBuffer[TreeNode]
            newForest.appendAll(leftForest)
            newForest.appendAll(rightForest)

            if (leftChildConnectedNode != null) {
              newForest.append(leftChildConnectedNode)
            }

            if (rightChildConnectedNode != null) {
              newForest.append(rightChildConnectedNode)
            }


            (newForest.toList,null) //cuurent node is not connected to parent
          }else {
            if (leftChildConnectedNode == null)  {
              curretNode.left = null
            }

            if (rightChildConnectedNode == null) {
              curretNode.right = null
            }
            (leftForest ++ rightForest,curretNode)
          }
        }
      }
    }

    val retValue = collect(root)
    if (retValue._2 != null) {
      retValue._2 :: retValue._1
    }else {
      retValue._1
    }

  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    root.left = new TreeNode(2)
    root.right = new TreeNode(3)
    root.left.left =  new TreeNode(4)
    root.left.right = new TreeNode(5)
    root.right.left = new TreeNode(6)
    root.right.right = new TreeNode(7)


    val retValue = delNodes(root,Array(3,5))
    println(retValue.mkString(","))
  }
}