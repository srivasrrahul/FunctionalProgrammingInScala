import java.util.concurrent.{Callable, Executors}
import java.util.concurrent.locks.ReentrantLock

import scala.collection.mutable.ListBuffer

case class BlockingQueue(val size : Int) {
  val lock = new ReentrantLock()
  val condFull = lock.newCondition()
  val condEmpty = lock.newCondition()
  val lst = new ListBuffer[Int]

  def enq(item : Int) : Unit = {

    try {
      lock.lock()
      while (lst.size == size) {
        println("Waiting for free space")
        condFull.await()
      }

      lst.append(item)
      condEmpty.signalAll()
    }finally  {
      lock.unlock()
    }
  }

  def dequeue() : Int = {

    var retValue = 0
    try {
      lock.lock()
      while (lst.isEmpty) {
        condEmpty.await()
      }

      retValue = lst.head
      lst.dropInPlace(1)
      condFull.signalAll()
    } finally {
      lock.unlock()
    }

    retValue
  }
}

case class Worker(val bq : BlockingQueue) extends Callable[Int] {
  override def call(): Int = {
    for (j <- 0 to 100000) {
      //Thread.sleep(100)
      bq.enq(j)
    }

    0

  }
}
object Solution {
  def main(args: Array[String]): Unit = {
    val blockingQueue = new BlockingQueue(10)
    val service = Executors.newCachedThreadPool()
    val future = service.submit(new Worker(blockingQueue))
    val future1 = service.submit(new Worker(blockingQueue))
    val future2 = service.submit(new Worker(blockingQueue))
    val future3 = service.submit(new Worker(blockingQueue))

    while (true) {
      //Thread.sleep(10)
      println(blockingQueue.dequeue())
    }
  }
}