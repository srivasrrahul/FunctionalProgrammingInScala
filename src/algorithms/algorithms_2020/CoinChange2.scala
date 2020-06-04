import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class ArrayWrapper(val arr : Array[Int]) {
  override def hashCode(): Int = {
    java.util.Arrays.hashCode(arr)
  }

  override def equals(obj: Any): Boolean = {
    java.util.Arrays.equals(arr,obj.asInstanceOf[ArrayWrapper].arr)

  }
}
object Solution {
  def change(amount: Int, coins: Array[Int]): Int = {
    if (amount == 0) {
      1
    } else {
      val cache = new mutable.HashMap[Int,mutable.HashSet[ArrayWrapper]]()
      def itr(currentAmount: Int): mutable.HashSet[ArrayWrapper] = {
        if (currentAmount > 0) {
          if (cache.contains(currentAmount)) {
            println("Cahce hit")
            cache.get(currentAmount).get
          }else {
            val lstBuffer = new mutable.HashSet[ArrayWrapper]
            for (coin <- coins) {
              val diff = currentAmount - coin
              if (diff == 0) {
                lstBuffer.add(new ArrayWrapper(Array(coin)))
              } else {
                if (diff > 0) {
                  val pendingLsts = itr(diff)
                  for (pendingLst <- pendingLsts) {
                    val arr1 = pendingLst.arr ++ Array(coin)
                    arr1.sortInPlace()


                    lstBuffer.add(new ArrayWrapper(arr1))
                  }
                }
              }
            }


            cache += ((currentAmount, lstBuffer))
            lstBuffer
          }
        } else {
          new mutable.HashSet()
        }
      }

      val pendingLsts = itr(amount)
      pendingLsts.size
      //println("Set Contents")
      //    for (s <- set) {
      //      println(" " + s.arr.mkString(","))
      //    }
      //set.size
      //1
    }
  }

  def main(args: Array[String]): Unit = {
    println(change(500,Array(1,2,5)))
    //1- 1
    //2 - 1,1  2
    //3 - 1,1,1 1,2
    //3 - 1 + f(2) 1,1,1 1,2
    //4 - 1+ f(3) 1,1,1,1 1,1,2    2 + f(2) 2,1,1
    // 1
  }
}