import scala.collection.mutable.ListBuffer

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  def findLeaves(root: TreeNode): List[List[Int]] = {

    def isLeaf(current : TreeNode) : Boolean = {
      current.left == null && current.right == null
    }

    def collectLeaves(current : TreeNode,lst : ListBuffer[Int]) : Unit = {
      (current.left != null,current.right != null) match {
        case (false,false) => {
          //is leaf //error
        }
        case (true,false) => {
          if (isLeaf(current.left)) {
            lst.addOne(current.left.value)
            current.left = null
          }else {
            collectLeaves(current.left,lst)
          }
        }
        case (false,true) => {
          if (isLeaf(current.right)) {
            lst.addOne(current.right.value)
            current.right = null
          }else {
            collectLeaves(current.right,lst)
          }

        }
        case (true,true) => {
          if (isLeaf(current.left)) {
            lst.addOne(current.left.value)
            current.left = null
          }else {
            collectLeaves(current.left,lst)
          }

          if (isLeaf(current.right)) {
            lst.addOne(current.right.value)
            current.right = null
          }else {
            collectLeaves(current.right,lst)
          }


        }
      }
    }

    val collectionLeaves = new ListBuffer[List[Int]]
    if (root != null) {
      while (isLeaf(root) == false) {
        val leafBuffer = new ListBuffer[Int]
        collectLeaves(root, leafBuffer)
        collectionLeaves.addOne(leafBuffer.toList)
      }

      collectionLeaves.addOne(List(root.value))
    }

    collectionLeaves.toList
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    val l = new TreeNode(2)
    val r = new TreeNode(3)
    val ll = new TreeNode(4)
    val lr = new TreeNode(5)

    root.left = l
    root.right = r

    l.left = ll
    l.right = lr


    println(findLeaves(null).mkString(","))

  }
}