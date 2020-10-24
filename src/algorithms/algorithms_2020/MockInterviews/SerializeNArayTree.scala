import scala.collection.mutable.ListBuffer

class Node(var _value: Int) {
  var value: Int = _value
  var neighbors: List[Node] = List()

  override def toString = s"Node(value=$value)"
}


class Codec {
  // Encodes a tree to a single string.
  def serialize(root: Node): String = {
    def itr(node : Node) : String = {
      if (node == null) {
        ""
      }else {
        val buffer = new StringBuilder
        buffer.append("(")
        buffer.append(node.value)
        for (neigbour <- node.neighbors) {
          buffer.append(",")
          val next = itr(neigbour)
          buffer.append(next)
        }

        buffer.append(")")
        buffer.toString()
      }
    }

    itr(root)
  }


  // Decodes your encoded data to tree.
  def deserialize(data: String): Node = {
    def itr(index : Int) : (Node,Int) = {
      if (index >= data.length) {
        (null,data.length)
      }else {

        var j = index
        j = j + 1 //eat up "("

        val valBuffer = new StringBuilder
        while (data(j) != ',' && data(j) != ')') {
          //println(j + " " + data(j))
          valBuffer.append(data(j))
          j = j + 1
        }

        val value = valBuffer.toString().toInt
        if (data(j) == ')') {
          (new Node(value),j+1) //parse the next
        }else {
          assert(data(j) == ',')
          val lstBuffer = new ListBuffer[Node]

          var found = true
          while (found) {
            val (node,nextStartToken) = itr(j+1)
            if (node == null || nextStartToken >= data.length) {
              found = false
            }else {
              lstBuffer.append(node)
              j = nextStartToken
              if (data(j) != ',') {
                found = false
              }
            }
          }

          val node = new Node(value)
          node.neighbors = lstBuffer.toList
          (node,j+1)

        }

      }

    }

    val (root,len) = itr(0)
    root
  }


}

object Solution {
  def main(args: Array[String]): Unit = {
    val codec = new Codec
    val root = new Node(12)
    val lr = new Node(13)
    lr.neighbors = List(new Node(14),new Node(15),new Node(16))
    root.neighbors = List(lr,new Node(17))
    println(root.neighbors)
    println(root.neighbors.head.neighbors)
    val s = codec.serialize(root)
    println(s)
    val dRoot = codec.deserialize(s)
    println(dRoot.neighbors)
    println(dRoot.neighbors.head.neighbors)

  }
}

