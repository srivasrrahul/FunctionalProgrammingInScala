import scala.collection.mutable.ArrayBuffer

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

object Solution {
  def isLeaf(node : TreeNode) : Boolean = {
    node.left == null && node.right == null
  }
  def maxPathSum(root: TreeNode): Int = {

    val sumArr = new ArrayBuffer[Int]()

    def itr(node : TreeNode) : (Int,Int) = {
      if (isLeaf(node)) {
        //println("Is leaf " + node.value)
        sumArr.addOne(node.value)
        (node.value,node.value)
      }else {
        var leftNodeResult : Option[(Int,Int)] = None

        if (node.left != null) {
          leftNodeResult = Some(itr(node.left))
        }


        var rightNodeResult : Option[(Int,Int)] = None
        if (node.right != null) {
          rightNodeResult = Some(itr(node.right))
        }

        (leftNodeResult,rightNodeResult) match {
          case (Some((leftTerminateMax,leftIncludeMax)),Some((rightTerminateMax,rightIncludeMax)))=> {

            val nodeAlone = node.value

            val nodeWithLeft = node.value + leftTerminateMax

            val nodeWithRight = node.value + rightTerminateMax

            val selfTerminateMax = scala.math.max(nodeAlone,scala.math.max(nodeWithLeft,nodeWithRight))

            val selfIncludeMax = leftTerminateMax + node.value + rightTerminateMax

            sumArr.addOne(selfTerminateMax)
            sumArr.addOne(selfIncludeMax)

            (selfTerminateMax,selfIncludeMax)

          }
          case (Some((leftTerminateMax,leftIncludeMax)),None) => {
            val nodeAlone = node.value

            val nodeWithLeft = node.value + leftTerminateMax

            val selfTerminateMax = scala.math.max(nodeAlone,nodeWithLeft)

            val selfIncludeMax = leftTerminateMax + node.value

            sumArr.addOne(selfTerminateMax)
            sumArr.addOne(selfIncludeMax)

            (selfTerminateMax,selfIncludeMax)



          }

          case (None,Some((rightTerminateMax,rightIncludeMax))) => {
            val nodeAlone = node.value

            val nodeWithRight = node.value + rightTerminateMax

            val selfTerminateMax = scala.math.max(nodeAlone,nodeWithRight)

            val selfIncludeMax = node.value + rightTerminateMax

            sumArr.addOne(selfTerminateMax)
            sumArr.addOne(selfIncludeMax)

            (selfTerminateMax,selfIncludeMax)


          }
          case _ => {
            (-1,-1)
          }
        }

      }
    }

    itr(root)

    //println(sumArr)
    sumArr.max

  }

  def main(args: Array[String]): Unit = {
//    val rootNode = new TreeNode(1)
//    val leftNode = new TreeNode(2)
//    val rightNode = new TreeNode(3)

    val rootNode = new TreeNode(-10)
    val leftNode = new TreeNode(9)
    val rightNode = new TreeNode(20)
    val lrightNode = new TreeNode(15)
    val rrightNode = new TreeNode(7)

    rootNode.left = leftNode
    rootNode.right = rightNode

    rightNode.left = lrightNode
    rightNode.right = rrightNode

    println(maxPathSum(rootNode))
  }
}