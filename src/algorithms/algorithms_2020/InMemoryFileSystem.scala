import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class MyFile  {
  val content = new StringBuilder
  def addContent(string: String) : Unit = {
    content.append(string)
  }

  def getContent() : String = {
    content.toString()
  }


}

class MyDirectory {

  val dirPointers = new mutable.TreeMap[String,MyDirectory]()
  val filePointers = new mutable.TreeMap[String,MyFile]

  def addDirectory(dirName : String) : Unit = {
    if (dirPointers.contains(dirName) == false) {
      dirPointers += ((dirName,new MyDirectory))
    }
  }

  def addFile(fileName : String) : MyFile = {
    filePointers += ((fileName,new MyFile))
    filePointers.get(fileName).get
  }

  def getSubDir(name : String) : MyDirectory= {
    dirPointers.get(name).get
  }

  def getFilePointer(name : String) : Option[MyFile] = {
    filePointers.get(name)
  }

  def list(name : String) : List[String] = {
    if (filePointers.contains(name)) {
      //File points
      List(name)
    }else {
      val lstBuffer = new mutable.TreeSet[String]()
      lstBuffer.addAll(dirPointers.keys)
      lstBuffer.addAll(filePointers.keys)
      lstBuffer.toList
    }
  }

  override def toString = s"MyDirectory($dirPointers, $filePointers)"
}
class FileSystem() {

  val root = new MyDirectory
  def lastDir(arr : Array[String]) : MyDirectory = {
    if (arr.size <= 2 ) {
      root
    }else {
      var current = root
      for (j <- 1 to arr.length-2) {
        current = current.getSubDir(arr(j))
      }

      current
    }
  }
  def ls(path: String): List[String] = {
    val pathLst = path.split("/")
    val current = lastDir(pathLst)

    if (path == "/") {
      current.list("")
    }else {
      println("Last " + current)
      current.list(pathLst.last)
    }

  }

  def mkdir(path: String) : Unit =  {
    val pathLst = path.split("/").tail
    var current = root
    for (level <- pathLst) {
      if (level.isEmpty == false) {
        current.addDirectory(level)
        current = current.getSubDir(level)
      }
    }
  }

  def addContentToFile(filePath: String, content: String) : Unit = {
    val arr = filePath.split("/")
    val current = lastDir(arr)

    val fp = current.getFilePointer(arr.last)
    fp match {
      case None => {
        current.addFile(arr.last).addContent(content)
      }
      case Some(filePointer) => {
        filePointer.addContent(content)
      }
    }


  }

  def readContentFromFile(filePath: String): String = {
    val arr = filePath.split("/")
    val current = lastDir(arr)

    val fp = current.getFilePointer(arr.last).get
    fp.getContent()

  }

}

object Solution {
  def main(args: Array[String]): Unit = {
    val fileSystem = new FileSystem
    println(fileSystem.ls("/"))
    fileSystem.mkdir("/a/b/c")
    fileSystem.addContentToFile("/a/b/c/d","hello")
    println(fileSystem.readContentFromFile("/a/b/c/d"))
    println(fileSystem.ls("/a/b/c"))
    //println(fileSystem.root)
  }
}
