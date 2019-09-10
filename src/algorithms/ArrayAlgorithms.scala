

case class ResTuple(zeros : Int , ones : Int)

object ArrayAlgorithms {




  def find_min[T](arr: Array[T])(implicit ev : T => Ordered[T]) : T = {
    var min_val = arr(0)
    for (n <- 1 to arr.length-1) {
      if (arr(n) < min_val) {
        min_val = arr(n)
      }
    }

    min_val
  }




  def main(args: Array[String]): Unit = {
    println(find_min(Array[Int](2,123,123,22,3,-1)))
  }

}