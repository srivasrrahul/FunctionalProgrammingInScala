import scala.collection.Searching

trait Direction
case object LeftDirection extends Direction
case object RightDirection extends Direction

object Solution {
  def searchRange(nums: Array[Int], target: Int): Array[Int] = {

    def searchLeftDir(beginIndex : Int,endIndex : Int) : Option[Int] = {
      //println("Left index " + beginIndex + " " + endIndex)
      val compare = beginIndex == endIndex
      compare match {
        case true => {
          //println("test1")
          if (nums(0) == target) {
            Some(0)
          }else {
            None
          }
        }
        case _ => {

          //println("test")
          val searchResult = nums.search(target, beginIndex, endIndex+1)
          searchResult match {
            case Searching.Found(foundIndex) => {
              //println("Found again")
              searchLeftDir(beginIndex, foundIndex - 1)  match {
                case Some(leftMoreIndex) => Some(leftMoreIndex)
                case None => Some(foundIndex)
              }
            }
            case _ => None
          }
        }
      }

    }

    def searchRightDir(beginIndex : Int,endIndex : Int) : Option[Int] = {
      //println("Riggjt + " + beginIndex + " " + endIndex)
      val compare = beginIndex == endIndex
      compare match {
        case true => {
          if (nums(beginIndex) == target) {
            Some(beginIndex)
          }else {
            None
          }
        }
        case _ => {
          val searchResult = nums.search(target, beginIndex, endIndex+1)
          searchResult match {
            case Searching.Found(foundIndex) => {
              searchRightDir(foundIndex+1, endIndex)  match {
                case Some(rightMoreIndex) => Some(rightMoreIndex)
                case _ => Some(foundIndex)
              }
            }
            case _ => None
          }
        }
      }

    }


    nums.search(target) match {
      case Searching.Found(foundIndex) => {
        //println("first flound " + foundIndex)
        val leftMost = searchLeftDir(0,foundIndex-1)
        val rightMost = searchRightDir(foundIndex+1,nums.length-1)

        (leftMost,rightMost) match {
          case (None,None) => {
            Array(foundIndex,foundIndex)
          }
          case (Some(leftIndex),None) => {
            Array(leftIndex,foundIndex)
          }
          case (None,Some(rightIndex)) => {
            Array(foundIndex,rightIndex)
          }
          case (Some(leftIndex),Some(rightIndex)) => {
            Array(leftIndex,rightIndex)
          }
        }
      }
      case _ => {
        Array(-1,-1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    println(searchRange(Array(1,1,1,1,1,1,2,3,4,4,5,5,5,6,7,8,8,8,8),8).mkString(","))
  }
}