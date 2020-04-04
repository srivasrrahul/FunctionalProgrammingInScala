

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null


  override def toString = s"TreeNode($value, $left, $right)"
}

import scala.collection.mutable.ListBuffer
trait FullBinaryTree
case object LeafNode extends FullBinaryTree
case class InternalNode(val left : FullBinaryTree,val right: FullBinaryTree) extends FullBinaryTree
object Solution {

  def convert(binaryTree: FullBinaryTree) : TreeNode = {
    binaryTree match {
      case LeafNode => {
        new TreeNode(0)
      }
      case InternalNode(left,right) => {
        val treeNode = new TreeNode(0)
        treeNode.left = convert(left)
        treeNode.right = convert(right)
        treeNode
      }
    }
  }

  def createList(someLst : Option[List[FullBinaryTree]]) : List[TreeNode] = {

    someLst match {
      case None => {
        List[TreeNode]()
      }
      case Some(lst : List[FullBinaryTree]) =>  {
        val retValue = new ListBuffer[TreeNode]
        lst.foreach(tree => {
          retValue.addOne(convert(tree))
        })

        retValue.toList
      }
    }
  }
  def allPossibleFBT(N: Int): List[TreeNode] = {
    N match {
      case 1 => List(new TreeNode(0))
      case 2 => List[TreeNode]()
      case 3 => {
        val root = new TreeNode(0)
        root.left = new TreeNode(0)
        root.right = new TreeNode(0)
        List(root)
      }
      case 4 => List[TreeNode]()
      case _ => {
        val arr = new Array[Option[List[FullBinaryTree]]](N+1)
        arr(1) = Some(List(LeafNode))
        arr(2) = None
        arr(3) = Some(List(new InternalNode(LeafNode,LeafNode)))
        arr(4) = None

        for (j <- 5 to N) {

          if ((j & (j-1)) == 0) {
            arr(j) = None
          }else {

            var leftCount = 1
            var rightCount = j-2

            val trees = new ListBuffer[FullBinaryTree]
            while (leftCount <= j-2 && rightCount >= 1) {
              val left = arr(leftCount)
              val right = arr(rightCount)
              (left,right) match {
                case (None,_) => {

                }
                case (_,None) => {

                }
                case (Some(llist : List[FullBinaryTree]),Some(rList : List[FullBinaryTree])) => {

                  llist.foreach(lTree => {
                    rList.foreach(rTree => {
                      trees.addOne(new InternalNode(lTree,rTree))
                    })
                  })


                }
              }

              leftCount = leftCount + 1
              rightCount = rightCount - 1
            }

            arr(j) = Some(trees.toList)
          }
        }

        //println(arr(N).get.mkString("\n"))
        createList(arr(N))
      }
    }


  }

  def main(args: Array[String]): Unit = {
    println(allPossibleFBT(7).mkString("\n"))
  }
}