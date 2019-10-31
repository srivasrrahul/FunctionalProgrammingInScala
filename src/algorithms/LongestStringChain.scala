import scala.collection.mutable

import util.control.Breaks._





class Predecessor {
  def predecessor(w1 : String,w2 : String) : Boolean = {
    //w1's length is less than w2

    if (w1.length > w2.length) {
      //predecessor(w2,w1)
      false
    }else {
      if (w1.length+1 == w2.length) {
        var isSkipped = false
        var pred = true
        var j = 0
        breakable {
          for (i <- 0 to w1.length - 1) {
            if (w1.charAt(i) == w2.charAt(j)) {
              j = j + 1
            } else {
              if (isSkipped == false) {
                isSkipped = true
                j = j + 1
                if (w1.charAt(i) != w2.charAt(j)) {
                  pred = false
                  break()
                }else {
                  j = j + 1
                }
              } else {
                pred = false
                break
              }
            }
          }
        }

        pred
      }else {
        false
      }

    }
  }
}

object Solution {
  def longestStrChain(words: Array[String]): Int = {
    scala.util.Sorting.quickSort(words)(new Ordering[String] {
      override def compare(x: String, y: String): Int = {
        x.length.compareTo(y.length)
      }
    })
    val pred = new Predecessor
    val chain = new Array[Int](words.length)
    chain(0) = 1
    for (j <- 1 to words.length-1) {
      var maxChain = 1
      for (k <- j-1 to 0 by -1) {
        if (pred.predecessor(words(k),words(j))) {
          val currentChain = chain(k) + 1
          if (currentChain > maxChain) {
            maxChain = currentChain
          }
        }
      }

      chain(j) = maxChain
    }

    //print(chain.mkString(","))
    chain.max
  }
  def main(args: Array[String]): Unit = {
    val tr = new Predecessor
    //println(tr.predecessor("bda","cbda"))
    println("String is " + longestStrChain(Array("ksqvsyq","ks","kss",
      "czvh","zczpzvdhx","zczpzvh","zczpzvhx","zcpzvh",
      "zczvh","gr","grukmj","ksqvsq","gruj","kssq",
      "ksqsq","grukkmj","grukj","zczpzfvdhx","gru")))
  }
}



