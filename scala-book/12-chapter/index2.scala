import scala.collection.mutable.ArrayBuffer

abstract class IntQueue {
  def get(): Int
  def put(x: Int)
}

class BasicIntQueue extends IntQueue {
  private val buf = new ArrayBuffer[Int]
  def get() = buf.remove(0)
  def put(x: Int) {
    buf += x
  }
}

// This trait defines a superclass, meaning it can only be mixed into
// class which extend this base

trait Doubling extends IntQueue {
  abstract override def put(x: Int) { super.put(2 * x) }
}

trait Incrementing extends IntQueue {
  abstract override def put(x: Int) { super.put(x + 1) }
}

trait Filtering extends IntQueue {
  abstract override def put(x: Int) {
    if (x >= 0) super.put(x)
  }
}

// Order of Trait is Right to Left
class WeirdQueue extends BasicIntQueue
  with Doubling
  with Incrementing
  with Filtering

object Main extends App {
  val q = new WeirdQueue
  q.put(1)
  q.put(-20)
  q.put(3)
  println(q.get())
  println(q.get())
}
