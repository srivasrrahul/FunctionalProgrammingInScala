import scala.collection.mutable.ListBuffer
import util.control.Breaks._





object Solution {

  type InternalNodeSum = Int

  type MaxLeaf = Int

  def mctFromLeafValues(arr: Array[Int]): Int = {

    val internalNodeSum = Array.ofDim[(InternalNodeSum,MaxLeaf)](arr.length,arr.length)
    for (j <- 0 to arr.length-1) {
      internalNodeSum(j)(j) = (0,arr(j))
    }


    var j = 0
    var k = 1
    var bigK = 2
    breakable {
      while (j < arr.length && k < arr.length) {
        //println(" j = " + j + " k = " + k)

        var minSum = (Int.MaxValue,Int.MinValue)
        for (i <- j to k-1) {
          val x = internalNodeSum(j)(i)
          val y = internalNodeSum(i+1)(k)

          val sampleSum = x._1 + y._1 + (x._2*y._2)
          val sampleMaxLeaf = scala.math.max(x._2,y._2)

          if (sampleSum < minSum._1) {
            minSum = (sampleSum,sampleMaxLeaf)
          }
        }

        internalNodeSum(j)(k) = minSum


        j += 1
        k += 1

        //println("Incremented j = " + j + " k = " + k)

        if (j >= arr.length && k >= arr.length) {
          //println("breaking")
          break
        }


        if (k >= arr.length) {
          //println("Resetting")
          j = 0
          k = bigK
          bigK += 1
        }
      }
    }

    val s = internalNodeSum.length

//    for (j <- 0 to s-1) {
//      for (k <- 0 to s-1) {
//        print(internalNodeSum(j)(k) + ",")
//      }
//      println()
//    }

    internalNodeSum(0)(arr.length-1)._1


  }

  def main(args: Array[String]): Unit = {
    println(mctFromLeafValues(Array(15,13,5,3,15)))

//    val tree = new InternalNode(new LeafNode(5),new LeafNode(7))
//    val second = combine(tree,3)
//    val all = combine(second.tail.head,2)
//    all.foreach(tree => {
//      println(eval(tree)._1)
//    })
    //println(second.mkString("\n"))
    //println("Input = " + second)

    //println(combine(second.tail.head,2).mkString("\n"))
  }


}