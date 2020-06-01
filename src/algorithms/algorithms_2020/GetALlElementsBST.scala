import scala.collection.mutable.ListBuffer

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def getAllElements(root1: TreeNode, root2: TreeNode): List[Int] = {
    def getList(node : TreeNode) : List[Int] = {
      if (node == null) {
        List()
      }else {
        getList(node.left) ++ List(node.value) ++ getList(node.right)
      }
    }

    val lst1 = getList(root1)
    val lst2 = getList(root2)

    var itr1 = lst1
    var itr2 = lst2

    val solution = new ListBuffer[Int]

    while (itr1 != Nil && itr2 != Nil) {
      if (itr1.head < itr2.head) {
        solution.append(itr1.head)
        itr1 = itr1.tail
      }else {
        if (itr2.head < itr1.head) {
          solution.append(itr2.head)
          itr2 = itr2.tail
        }else {
          //equal
          solution.append(itr1.head)
          solution.append(itr2.head)
          itr1 = itr1.tail
          itr2 = itr2.tail

        }
      }
    }

    while (itr1 != Nil) {
      solution.append(itr1.head)
      itr1 = itr1.tail
    }

    while (itr2 != Nil) {
      solution.append(itr2.head)
      itr2 = itr2.tail
    }

    solution.toList
  }



  def main(args: Array[String]): Unit = {
    val root = new TreeNode(10)
    root.left = new TreeNode(5)
    root.right = new TreeNode(15)

    val root1 = new TreeNode(12)
    root1.left = new TreeNode(3)
    root1.right = new TreeNode(16)
    root1.left.left = new TreeNode(2)

    println(getAllElements(root,root1))
  }
}