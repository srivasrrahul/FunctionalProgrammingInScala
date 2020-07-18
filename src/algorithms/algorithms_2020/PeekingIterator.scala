// Scala Iterator reference:
// https://www.scala-lang.org/api/2.12.2/scala/collection/Iterator.html

class PeekingIterator( private val itr: Iterator[Int]) extends Iterator[Int] {

  var current = itr
  def peek(): Int = {
    val (newCurrent,peekingIterator) = current.duplicate
    current = newCurrent
    peekingIterator.next()
  }

  override def next(): Int = {
    current.next()
  }

  override def hasNext(): Boolean = {
    current.hasNext
  }
}