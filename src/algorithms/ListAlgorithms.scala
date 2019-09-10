object ListAlgorithms {

//  def isSorted(lst : List[Int]) : Boolean = {
//    def min_max_lst(l : List[Int]) : (Boolean,(Int,Int)) = l match {
//      case x::Nil => {
//        (true,(x,x))
//      }
//      case x::xs => {
//        val (sorted_sub,(min_sub,max_sub)) = min_max_lst(xs)
//        if (sorted_sub && x <= min_sub) {
//          (true,(x,max_sub))
//        }else {
//          (false,(0,0))
//        }
//      }
//    }
//
//    val (ret_val,_) = min_max_lst(lst)
//    ret_val
//  }
//
//  def main(args: Array[String]): Unit = {
//    val l = List(100,200,299,300,300,400,500,700,800)
//    println(isSorted(l))
//  }
}
