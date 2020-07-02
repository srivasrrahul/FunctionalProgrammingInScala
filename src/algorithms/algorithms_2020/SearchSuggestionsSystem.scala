import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Solution {
  def suggestedProducts(products: Array[String], searchWord: String): List[List[String]] = {
    products.sortInPlace()
    val retValue = new ListBuffer[List[String]]
    for (j <- 0 to searchWord.length-1) {
      val searchString = searchWord.substring(0,j+1)
      //println(searchString)
      val lstBuffer = new ListBuffer[String]
      for (product <- products if lstBuffer.size < 3) {
        if (product.startsWith(searchString)) {
          lstBuffer.append(product)
        }
      }


      retValue.append(lstBuffer.toList)
    }

    retValue.toList
  }

  def main(args: Array[String]): Unit = {
    println(suggestedProducts(Array("mobile","mouse","moneypot","monitor","mousepad"),"mouse"))
  }
}