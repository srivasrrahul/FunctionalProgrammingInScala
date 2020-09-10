import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class TreeNode(var range: Range,var left : TreeNode = null,var right : TreeNode = null) {
  val value = range
  def leftMax() : Int = {
    if (left != null) {
      left.range.max
    }else {
      Int.MinValue
    }
  }

  def rightMax() : Int = {
    if (right != null) {
      right.range.max
    }else {
      Int.MinValue
    }
  }

  def refresh() : Unit = {
    val begin = value.head
    val end = math.max(value.max,math.max(leftMax(),rightMax()))
    range = Range(begin,end+1)
  }


  override def toString = s"TreeNode($value, $range)"
}

class IST {
  def isLeaf(node: TreeNode) : Boolean = {
    node.left == null && node.right == null
  }

  def createIST(x : Range) : TreeNode = {
    new TreeNode(x)
  }

  def addInterval(range : Range,root : TreeNode) : Unit = {
    println("Adding interval " + range)
    def add(node : TreeNode) : TreeNode = {
      if (node == null) {
        val newNode = new TreeNode(range)
        newNode
      }else {
        if (node.range.head == range.head) {
          //duplicate

        }else {
          if (node.range.head > range.head) {
            node.left = add(node.left)
          }else {
            node.right = add(node.right)
          }
        }

        node.refresh()
        node
      }
    }

    add(root)
  }

  def overlap(r1 : Range,r2 : Range) : Boolean = {
    //one point common is common
    r1.contains(r2.head) || r1.contains(r2.last)
  }
  def checkIntervalMatch(range: Range,root : TreeNode) : Option[TreeNode] = {
    def check(node : TreeNode) : Option[TreeNode] = {
      if (node == null) {
        None
      }else {
        if (overlap(node.value,range)) {
          Some(node)

        }else {
          var leftFound = false
          if (node.left == null) {
            check(node.right)
          }else {
            if (node.left.range.last < range.head) {
              check(node.right)
            }else {
              check(node.left)
            }
          }

          //node.refresh()
        }
      }


    }
    check(root)
  }

  def findLargest(node : TreeNode) : TreeNode = {
    null
  }

  //deletes any matching interval
  def deleteInterval(range : Range,root : TreeNode) : (Option[TreeNode],TreeNode) = {
    var deletedNode : Option[TreeNode] = None
    def checkAndDelet(node : TreeNode) : TreeNode = {
      if (node == null) {
        null
      }else {
        if (overlap(node.value,range)) {
          deletedNode = Some(node)
          if (isLeaf(node)) {
            null
          }else {
            if (node.left == null) {
              node.right
            }else {
              if (node.right == null) {
                node.left
              }else {
                if (node.left.right == null) {
                  node.left.right = node.right
                  node.left.refresh()
                  node.left
                }else {
                  var pLargestNode = node.left
                  while (pLargestNode.right != null) {
                    pLargestNode = pLargestNode.right
                  }

                  val largest = pLargestNode.right
                  //lar
                  pLargestNode.right = null
                  pLargestNode.left = largest.left
                  pLargestNode.refresh()

                  largest.right = node.right
                  largest.left = node.left
                  largest.refresh()
                  largest
                }
              }
            }
          }
        }else {
          //var leftFound = false
          if (node.left == null) {
            node.right = checkAndDelet(node.right)
          }else {
            if (node.left.range.last < range.head) {
              node.right = checkAndDelet(node.right)
            }else {
              node.left = checkAndDelet(node.left)
            }
          }

          node.refresh()
          node
        }
      }
    }

    val newRoot = checkAndDelet(root)
    (deletedNode,newRoot)
  }

  def deleteAllContainedIntervals(range: Range,root : TreeNode) : TreeNode = {
    //Remove all matching intervals
    //add back pending intervals
    //returns newRoot
    var vRoot = root
    var found = false
    val matchingIntervals = new ArrayBuffer[Range]()
    do {
      found = false
      val (deletedNode,newRoot) = deleteInterval(range,vRoot)
      if (deletedNode.isDefined) {
        matchingIntervals.append(deletedNode.get.value)
        vRoot = newRoot
        found = true
      }
    } while (found == true)


    matchingIntervals.sortInPlace()(new Ordering[Range] {
      override def compare(x: Range, y: Range): Int = {
        x.head.compareTo(y.head)
      }
    })

    println(matchingIntervals)

    val mergedLst = new ListBuffer[Range]
    def merge(newRange : Range) : Unit = {
      if (newRange.head < mergedLst.last.last) {
        val lastElement = mergedLst.last
        mergedLst.dropRightInPlace(1)
        mergedLst.append(Range(lastElement.head,math.max(lastElement.end,newRange.end)))
      }else {
        mergedLst.append(newRange)
      }
    }


    if (matchingIntervals.size > 0) {
      mergedLst.append(matchingIntervals(0))
      for (j <- 1 to matchingIntervals.length-1) {
        merge(matchingIntervals(j))
      }

      println("deleting range " + range)

      println("merged lst " + mergedLst)
      val rangeAdded = new mutable.HashSet[Range]() //Need to make it random
      for (r <- mergedLst) {
        if (r.contains(range.head) || r.contains(range.last)) {
          if (r.head < range.head) {
            val first = r.head
            val last = range.head //yhis is not included
            rangeAdded.add(Range(first,last))
            //addInterval(Range(first,last),vRoot)

            if (range.last < r.last) {
              val f = range.last+1
              val l = r.end
              //addInterval(Range(f,l),vRoot)
              //println("Here " + Range(f,l))
              rangeAdded.add(Range(f,l))
            }
          }else {
            if (range.head < r.head) {
              val f = range.last+1
              val l = r.last+1
              //addInterval(Range(f,l),vRoot)

              rangeAdded.add(Range(f,l))
            }else {
              //both head are equal
              if (range.last < r.last) {
                val f = range.last+1
                val l = r.last+1
                //addInterval(Range(f,l),vRoot)

                rangeAdded.add(Range(f,l))
              }
            }
          }
        }
      }

      println("New Ranges " + rangeAdded)
      for (newRange <- rangeAdded) {
        if (vRoot == null) {
          vRoot = createIST(newRange)
        }else {
          addInterval(newRange, vRoot)
        }
      }
    }

    vRoot
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val root = new TreeNode(Range(30,41))
    val ist = new IST
    ist.addInterval(Range(10,21),root)
    ist.addInterval((Range(75,86)),root)
    ist.addInterval(Range(15,26),root)
    ist.addInterval(Range(35,38),root)
    ist.addInterval(Range(70,91),root)
    println(root.range)
    println("Root Left " + root.left.range)
    println("Root right " + root.right.range)
    println("Root left right " + root.left.right.range)
    println("Root right left  " + root.right.left.range)
    println("Root right left right " + root.right.left.right.range)

    //println(ist.checkIntervalMatch(Range(23,24),root))
    //println(ist.deleteInterval(Range(23,24),root)._1)
    //println(ist.deleteInterval(Range(23,24),root)._1)
//    println(ist.deleteInterval(Range(11,12),root)._1)
//    println("Root left " + root.left)

//    println(ist.deleteInterval(Range(86,88),root)._1)
//    println(ist.deleteInterval(Range(86,88),root)._1)
//    println("Root right " + root.right)
//    println("Root right left " + root.right.left)
//    println("Root " + root)

    var vRoot = root
    vRoot = ist.deleteAllContainedIntervals(Range(76,86),vRoot)
    println(vRoot.right)

  }
}