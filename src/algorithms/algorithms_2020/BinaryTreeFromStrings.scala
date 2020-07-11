class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def str2tree(s: String): TreeNode = {
    def parse(currentIndex : Int) : (TreeNode,Int) = {
      //currentIndex is never greater than s.length
      //it starts always with number and its potential left and right completion
      var strDigit = new StringBuilder
      var index = currentIndex
      println(index)
      while (index < s.length && (s(index).isDigit || s(index) == '-')) {
        strDigit.append(s(index))
        index = index + 1
      }

      val digit = Integer.parseInt(strDigit.toString())

      if (index < s.length) {
        //Now if index is finished
        s(index) match {
          case ')' => {
            (new TreeNode(digit), index)
          }
          case '(' => {
            //left exists
            index = index + 1
            val (leftTreeNode, leftParsedTill) = parse(index)
            index = leftParsedTill + 1
            //check for right
            if (index < s.length) {
              s(index) match {
                case ')' => {
                  (new TreeNode(digit, leftTreeNode), index)
                }
                case '(' => {
                  //right exists
                  index = index + 1
                  //println("For right " + index )
                  val (rightTreeNode, rightParsedTill) = parse(index)
                  index = rightParsedTill + 1 //accomodate next ')'
                  (new TreeNode(digit, leftTreeNode, rightTreeNode), index)
                }
              }
            } else {
              (new TreeNode(digit, leftTreeNode), index)
            }
          }
        }
      }else {
        (new TreeNode(digit), index)
      }
    }

    //println(s)
    if (s.isEmpty) {
      null
    }else {
      parse(0)._1
    }
  }

  def main(args: Array[String]): Unit = {
    val treeNode = str2tree("4")
    println(treeNode.value)
//    println(treeNode.left.value)
//    println(treeNode.left.left.value)
//    println(treeNode.right.value)
//    println(treeNode.right.left.value)
//    println(treeNode.right.right.value)
//    println(treeNode.left.left.value)
//    println(treeNode.left.right.value)
//    println(treeNode.right.value)
//    println(treeNode.right.left.value)
//    println(treeNode.value)
//    println(treeNode.left.value)
//    println(treeNode.right.value)
    //println(str2tree("4(2(3)(1))(6(5))"))
  }
}