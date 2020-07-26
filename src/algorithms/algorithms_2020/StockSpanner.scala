import scala.collection.mutable.ArrayBuffer

class StockSpanner() {

  val spanner = new ArrayBuffer[(Int,Int)]() //price and count consecutive
  def next(price: Int): Int = {
    //println("For price " + price + " " + spanner.mkString(","))
    if (spanner.length == 0) {
      spanner.append((price,1))
      1
    }else {
      var prevIndex = spanner.length-1 //2
      //var currentPrice = price //70
      var lastIsLess = true
      while (prevIndex >= 0 && lastIsLess == true ) {
        val (lastPrice,lastCount) = spanner(prevIndex) //60,1  80,1
        if (lastPrice <= price) { //true   false
          prevIndex = prevIndex - lastCount //2-1=1
          //currentPrice = lastPrice //60
        }else {
          lastIsLess = false
        }
      }

      val diffCount = (spanner.length - prevIndex) //3-1 = 2
      spanner.append((price,diffCount))
      diffCount
    }
  }


}

object Solution {
  def main(args: Array[String]): Unit = {
    val stockSpanner = new StockSpanner
    println(stockSpanner.next(100))
    println(stockSpanner.next(80))
    println(stockSpanner.next(60))
    println(stockSpanner.next(70))
    println(stockSpanner.next(60))
    println(stockSpanner.next(75))
    println(stockSpanner.next(85))
  }
}
