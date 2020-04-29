import scala.collection.mutable.ArrayBuffer

class TreeNode(var _value: Int) {
  var value: Int = _value
  var left: TreeNode = null
  var right: TreeNode = null
}

class Codec {
  def encodeInt(value : Int) : Array[Byte] = {
    var arr = new Array[Byte](4)
    val first = 0XFF & value
    val second = (value >> 8) & 0XFF
    val third = (value >> 16) & 0XFF
    val fourth = (value >> 24) & 0XFF

    arr(0) =  first.toByte

    arr(1) = second.toByte
    arr(2) = third.toByte
    arr(3) = fourth.toByte

    arr

  }

  def decodeInt(arr : Array[Byte],offset : Int) : Int = {
    var decodedDigit : Long = 0L
    decodedDigit = 0xFF & arr(offset + 3)
    //printf("%X\n",decodedDigit)
    decodedDigit = decodedDigit<<8
    decodedDigit = decodedDigit | (arr(offset+2) & 0XFF)
    //printf("%X\n",decodedDigit)
    decodedDigit = decodedDigit<<8
    decodedDigit = decodedDigit | (arr(offset + 1) & 0XFF)
    //printf("%X\n",decodedDigit)
    decodedDigit = decodedDigit<<8
//    //printf("%X  %X\n",decodedDigit,arr(offset+0))
//    //val y = arr(offset + 0) & 0XFF
//    printf("Long %X\n ",y)
    decodedDigit = decodedDigit | (arr(offset+0) & 0XFF)
//    printf("%X\n",decodedDigit)
//    println(decodedDigit.toInt)
    decodedDigit.toInt
  }
  // Encodes a list of strings to a single string.
  def serialize(root: TreeNode): String = {
    def serializeNode(node : TreeNode) : Array[Byte] = {
      node match {
        case null => {
          Array[Byte](0X00,0X00,0X00,0X00)
        }
        case _ => {
          val encodedValue = encodeInt(node.value)
          val encodedLeftNode = serializeNode(node.left)
          val encodedRightNode = serializeNode(node.right)

          val totalLen = encodedValue.length + encodedLeftNode.length + encodedRightNode.length
          //println("Total Len is " + totalLen)
          val encodedTotalLen = encodeInt(totalLen)
          val arrBuffer = new ArrayBuffer[Byte]()
          arrBuffer.addAll(encodedTotalLen)
          arrBuffer.addAll(encodedValue)
          arrBuffer.addAll(encodedLeftNode)
          arrBuffer.addAll(encodedRightNode)
          arrBuffer.toArray
        }
      }
    }
    val arr = serializeNode(root)
    val stringBuffer = new StringBuilder
    for (j <- 0 to arr.length-1) {
      val formattedStr = String.format("00%02X",arr(j))
      stringBuffer.append(formattedStr)
    }

    //println(arr.mkString(","))
    stringBuffer.toString
  }

  // Decodes a single string to a list of strings.
  def deserialize(s: String): TreeNode = {
    def deserializeNode(array: Array[Byte],offSet : Int) : (TreeNode,Int) = {
      //exrtract first four bytes
      val totalTreeLen = decodeInt(array,offSet)
      if (totalTreeLen == 0) {
        (null,offSet+4)
      }else {
        var treePointer = offSet + 4
        val nodeValue = decodeInt(array,treePointer)
        treePointer = treePointer + 4

        val (leftNode,leftNext) = deserializeNode(array,treePointer)
        treePointer = leftNext
        val (rightNode,rightNext) = deserializeNode(array,treePointer)
        treePointer = rightNext

        val newNode = new TreeNode(nodeValue)
        newNode.left = leftNode
        newNode.right = rightNode

        //println(totalTreeLen + " " + rightNext)
        (newNode,offSet + 4 + totalTreeLen)
      }
    }

    var arr = new ArrayBuffer[Byte]()
    var j = 0
    while (j < s.length) {
      val currentHexStr = s.substring(j,j+4)
      val byteVal = Integer.parseInt(currentHexStr,16)
      if (byteVal > 255) {
        println("Error")
      }
      arr.append(byteVal.toByte)
      //println("Adding " + byteVal)
      j = j + 4
    }
    //println("Dese")

    //println(arr.toArray.mkString(","))
    deserializeNode(arr.toArray,0)._1
    //null
  }
}

object Solution {
  def main(args: Array[String]): Unit = {
    val codec = new Codec

//    val x = 1222213131
//    printf("00%02X = \n",x)
//        val arr = codec.encodeInt(x)
//    for (j <- 0 to arr.length-1) {
//        printf("%X,",arr(j))
//      //println(arr.mkString(","))
//    }
    //println()
    //println(codec.decodeInt(arr,0))
//
//    val arr = codec.encodeInt(4)
//    println(arr.mkString(","))
//    println(codec.decodeInt(arr,0))
//
    val root = new TreeNode(1)
    root.left = new TreeNode(2)
    root.right = new TreeNode(3)
    val str = codec.serialize(root)
    //println(str)
    val rootCopy = codec.deserialize(str)
    println(rootCopy.value)
    println(rootCopy.left.value)
    println(rootCopy.right.value)
  }
}