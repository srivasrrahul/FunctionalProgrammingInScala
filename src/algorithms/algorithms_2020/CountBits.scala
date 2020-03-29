
object Solution {
  def countBits(num: Int): Array[Int] = {
    var count = new Array[Int](num+1)
    num match {
      case 0 => {

      }
      case 1 => {
        count(1) = 1
      }
      case _ => {
        count(1) = 1
        var diff = 0
        for (j <- 2 to num) {
          if ((j&(j-1)) == 0) {
            diff = 0
            count(j) = 1
          }else {
            diff += 1
            count(j) = 1 + count(diff)
          }
        }
      }
    }


    count
  }

  def main(args: Array[String]): Unit = {
    println(countBits(0).mkString(","))
  }
}