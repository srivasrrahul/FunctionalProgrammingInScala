import scala.collection.mutable.{ArrayBuffer, ListBuffer}

import scala.util.control.Breaks._

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  def rightSideView(root: TreeNode): List[Int] = {
    def explore(node: TreeNode): List[Int] = {
      if (node == null) {
        List[Int]()
      } else {
        if (node.left == null && node.right == null) {
          List(node.value)
        } else {
          val rightExplored = explore(node.right)
          val leftExplored = explore(node.left)

          //Eliminate left till right exists and then append

          val retValue = new ListBuffer[Int]

          retValue.append(node.value)
          retValue.appendAll(rightExplored)

          var rList = rightExplored
          var llist = leftExplored

          breakable {
            while (true) {
              (rList, llist) match {
                case (Nil, _) => {
                  break()
                }
                case (_, Nil) => {
                  break()
                }
                case (_, _) => {
                  rList = rList.tail
                  llist = llist.tail
                }
              }
            }
          }


          llist.foreach(x => {
            retValue.append(x)
          })

          retValue.toList

        }
      }
    }

    explore(root)
  }

  def main(args: Array[String]): Unit = {
    val root = new TreeNode(1)
    val l = new TreeNode(2)
    val r = new TreeNode(3)

    root.left = l
    root.right = r

    val ll = new TreeNode(4)
    val lr = new TreeNode(5)

    l.left = ll
    l.right = lr


    val rl = new TreeNode(6)
    val rr = new TreeNode(7)

    r.left = rl
    r.right = rr

    val lrl = new TreeNode(11)

    lr.left = lrl

    val lll = new TreeNode(8)
    val llr = new TreeNode(9)

    ll.left = lll
    ll.right = llr

    val llll = new TreeNode(10)

    lll.left = llll

    println(rightSideView(null))
  }
}