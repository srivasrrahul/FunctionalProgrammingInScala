import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def verticalOrder(root: TreeNode): List[List[Int]] = {
    val orderedList = new mutable.TreeMap[Int,mutable.TreeMap[Int,ListBuffer[Int]]]()
    def itr(currentNode : TreeNode,currentCol : Int,currentLevel : Int) : Unit = {
      currentNode match {
        case null => {

        }
        case _ => {
          val colMap = orderedList.getOrElseUpdate(currentCol,new mutable.TreeMap[Int,ListBuffer[Int]])
          val defaultList = colMap.getOrElseUpdate(currentLevel,new ListBuffer[Int])
          defaultList.append(currentNode.value)

          itr(currentNode.left,currentCol-1,currentLevel+1)

          itr(currentNode.right,currentCol+1,currentLevel+1)
        }
      }
    }

    itr(root,0,0)

    //println(orderedList)
    val lstBuffer = new ListBuffer[List[Int]]
    for ((c,colValues) <- orderedList) {
      val localLst = new ListBuffer[Int]
      for ((r,rowValue) <- colValues) {
        localLst.addAll(rowValue)
      }

      lstBuffer.append(localLst.toList)
    }

    lstBuffer.toList
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(3)
    root.left = new TreeNode(9)
    root.right = new TreeNode(8)
    root.left.left = new TreeNode(4)
    root.left.right = new TreeNode(0)
    root.right.left = new TreeNode(1)
    root.right.right = new TreeNode(7)

    println(verticalOrder(root))

  }
}