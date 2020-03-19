import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class TreeNode(var _value: Int) {
     var value: Int = _value
     var left: TreeNode = null
     var right: TreeNode = null
  }

sealed trait Order
case object LeftOrder extends Order
case object RightOrder extends Order

object Solution {
  def changeOrder(o : Order) : Order = {
    o match {
      case LeftOrder => {
        RightOrder
      }
      case RightOrder => {
        LeftOrder
      }
    }
  }
  def zigzagLevelOrder(root: TreeNode): List[List[Int]] = {
    val traversal = new ListBuffer[TreeNode]()
    if (root != null) {
      traversal.addOne(root)
    }

    val solution = new ListBuffer[List[Int]]
    var order : Order = RightOrder

    while (traversal.isEmpty == false) {
      println("Ordering is " + order)
      val lst = new ListBuffer[Int]
      var nextLevel = new ListBuffer[TreeNode]()



      val rev = traversal.reverse
      rev.foreach(node => {
        order match {
          case LeftOrder => {
            if (node.left != null) {
              nextLevel.addOne(node.left)
            }

            if (node.right != null) {
              nextLevel.addOne(node.right)
            }
          }
          case RightOrder => {
            if (node.right != null) {
              nextLevel.addOne(node.right)
            }

            if (node.left != null) {
              nextLevel.addOne(node.left)
            }
          }
        }

      })

      traversal.foreach(node => {
        lst.addOne(node.value)
      })

      traversal.clear()
      traversal.addAll(nextLevel)
      solution.addOne(lst.toList)
      order = changeOrder(order)


    }

    solution.toList
  }

  def main(args: Array[String]): Unit = {
    val r = new TreeNode(1)
    val l1 = new TreeNode(2)
    val l2 = new TreeNode(3)

    val l3 = new TreeNode(4)
    val l4 = new TreeNode(5)
    r.left = l1
    r.right = l2

    l1.left = l3
    l2.right = l4

    val s = zigzagLevelOrder(r)
    println(s.mkString(","))

  }
}