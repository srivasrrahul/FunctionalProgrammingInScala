import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class StrLen(val x : Int,val y : Int,val z : Int)
case class Index(val aC : Int,val bC : Int,val cC : Int)
object Solution {
  def longestDiverseString(a: Int, b: Int, c: Int): String = {
    val cache = new mutable.HashMap[Index,(String,StrLen)]()
    def largest(aC : Int,bC : Int,cC : Int) : (String,StrLen) = {
      if (aC == 0 && bC == 0 && cC == 0) {
        ("",new StrLen(0,0,0))
      }else {

        if (aC == 0 && bC == 0) {
          ("c"*(math.min(2,cC)),new StrLen(0,0,math.min(2,cC)))
        }else {
          if (bC == 0 && cC == 0) {
            ("a"*(math.min(2,aC)),new StrLen(math.min(2,aC),0,0))
          }else {
            if (aC == 0 && cC == 0) {
              ("b"*(math.min(2,bC)),new StrLen(0,math.min(2,bC),0))
            }else {
              val index = new Index(aC,bC,cC)
              if (cache.contains(index)) {
                cache.get(index).get
              }else {
                val retValue = new ListBuffer[(String, StrLen)]

                if (aC >= 1) {
                  var (a1Str, a1Len) = largest(aC - 1, bC, cC)
                  if (a1Str.size >= 2 && a1Str(0) == 'a' && a1Str(1) == 'a') {
                    //retValue.append(("a" ++ a1Str, new StrLen(a1Len.x + 1, a1Len.y, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("a" + a1Str, new StrLen(a1Len.x + 1, a1Len.y, a1Len.z)))
                  }

                  a1Str = a1Str.reverse
                  if (a1Str.size >= 2 && a1Str(0) == 'a' && a1Str(1) == 'a') {
                    //retValue.append(("a" ++ a1Str, new StrLen(a1Len.x + 1, a1Len.y, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("a" + a1Str, new StrLen(a1Len.x + 1, a1Len.y, a1Len.z)))
                  }
                }

                if (aC >= 2) {
                  var (a1Str, a1Len) = largest(aC - 2, bC, cC)
                  if (a1Str.size >= 1 && a1Str(0) == 'a') {
                    //retValue.append(("aa" ++ a1Str, new StrLen(a1Len.x + 2, a1Len.y, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("aa" + a1Str, new StrLen(a1Len.x + 2, a1Len.y, a1Len.z)))
                  }

                  a1Str = a1Str.reverse
                  if (a1Str.size >= 1 && a1Str(0) == 'a') {
                    //retValue.append(("aa" ++ a1Str, new StrLen(a1Len.x + 2, a1Len.y, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("aa" + a1Str, new StrLen(a1Len.x + 2, a1Len.y, a1Len.z)))
                  }
                }

                if (bC >= 1) {
                  var (a1Str, a1Len) = largest(aC, bC - 1, cC)
                  if (a1Str.size >= 2 && a1Str(0) == 'b' && a1Str(1) == 'b') {
                    //retValue.append(("b" ++ a1Str, new StrLen(a1Len.x, a1Len.y+1, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("b" + a1Str, new StrLen(a1Len.x, a1Len.y + 1, a1Len.z)))
                  }

                  a1Str = a1Str.reverse
                  if (a1Str.size >= 2 && a1Str(0) == 'b' && a1Str(1) == 'b') {
                    //retValue.append(("b" ++ a1Str, new StrLen(a1Len.x, a1Len.y+1, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("b" + a1Str, new StrLen(a1Len.x, a1Len.y + 1, a1Len.z)))
                  }

                }

                if (bC >= 2) {
                  var (a1Str, a1Len) = largest(aC, bC - 2, cC)
                  if (a1Str.size >= 1 && a1Str(0) == 'b') {
                    //retValue.append(("bb" ++ a1Str, new StrLen(a1Len.x, a1Len.y+2, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("bb" + a1Str, new StrLen(a1Len.x, a1Len.y + 2, a1Len.z)))
                  }

                  a1Str = a1Str.reverse
                  if (a1Str.size >= 1 && a1Str(0) == 'b') {
                    //retValue.append(("bb" ++ a1Str, new StrLen(a1Len.x, a1Len.y+2, a1Len.z)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("bb" + a1Str, new StrLen(a1Len.x, a1Len.y + 2, a1Len.z)))
                  }

                }

                if (cC >= 1) {
                  var (a1Str, a1Len) = largest(aC, bC, cC - 1)
                  if (a1Str.size >= 2 && a1Str(0) == 'c' && a1Str(1) == 'c') {
                    //retValue.append(("c" ++ a1Str, new StrLen(a1Len.x, a1Len.y, a1Len.z+1)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("c" + a1Str, new StrLen(a1Len.x, a1Len.y, a1Len.z + 1)))
                  }

                  a1Str = a1Str.reverse
                  if (a1Str.size >= 2 && a1Str(0) == 'c' && a1Str(1) == 'c') {
                    //retValue.append(("c" ++ a1Str, new StrLen(a1Len.x, a1Len.y, a1Len.z+1)))
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("c" + a1Str, new StrLen(a1Len.x, a1Len.y, a1Len.z + 1)))
                  }

                }

                if (cC >= 2) {
                  var (a1Str, a1Len) = largest(aC, bC, cC - 2)
                  if (a1Str.size >= 1 && a1Str(0) == 'c') {
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("cc" + a1Str, new StrLen(a1Len.x, a1Len.y, a1Len.z + 2)))
                  }

                  a1Str = a1Str.reverse
                  if (a1Str.size >= 1 && a1Str(0) == 'c') {
                    retValue.append((a1Str, a1Len))
                  } else {
                    retValue.append(("cc" + a1Str, new StrLen(a1Len.x, a1Len.y, a1Len.z + 2)))
                  }
                }


                val maxElement = retValue.max(new Ordering[(String, StrLen)] {
                  override def compare(x: (String, StrLen), y: (String, StrLen)): Int = {
                    x._1.size.compareTo(y._1.size)
                  }
                })

                cache += ((index,maxElement))
                maxElement

              }

            }
          }
        }
      }
    }

    val (largestStr,_) = largest(a,b,c)
    largestStr
  }

  def main(args: Array[String]): Unit = {
    println(longestDiverseString(2,2,1))
  }
}