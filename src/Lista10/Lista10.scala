package Lista10

class EmptyQueueException(msg: String) extends Exception(msg)
import scala.collection.mutable.Seq

class MyQueue[+T] private (private val qBegin: List[T], private val qEnd: List[T]):
  def enqueue[S >: T](x: S) =
    qBegin match
      case Nil => new MyQueue[S](List(x), Nil)
      case _ => new MyQueue[S](qBegin, x :: qEnd)

  def dequeue() =
    qBegin match
      case Nil => this
      case _ :: Nil =>
        if qEnd == Nil then new MyQueue[T](Nil, Nil)
        else new MyQueue[T](qEnd.reverse, Nil)
      case _ :: t => new MyQueue[T](t, qEnd)

  def first() =
    qBegin match
      case h :: _ => h
      case _ => throw new EmptyQueueException("Empty queue")

  def isEmpty() = qBegin == Nil

object MyQueue:
  def apply[T](xs: T*) = new MyQueue[T](xs.toList, Nil)
  def empty[T] = new MyQueue[T](Nil, Nil)



def copy[T](src: Seq[T], dest: Seq[T]) =
  require(dest.length >= src.length, "Destinated sequence cannot be shorter than source sequence!")
  var i = 0
  src.foreach(data =>
    dest.update(i, data)
    i += 1
  )

object Lista10 extends App:
  var myQueue: MyQueue[AnyVal] = MyQueue.empty
  //myQueue.first()
  myQueue = myQueue.dequeue()
  myQueue = myQueue.enqueue(5)
  println(myQueue.first())
  myQueue = myQueue.enqueue(10.0)
  myQueue = myQueue.enqueue(20.0f)
  myQueue = myQueue.enqueue(40)
  println(myQueue.first())
  myQueue = myQueue.dequeue()
  println(myQueue.first())
  myQueue = myQueue.dequeue()
  println(myQueue.first())
  myQueue = myQueue.dequeue()
  println(myQueue.first())

  var myQueue2: MyQueue[Int] = MyQueue.empty
  myQueue2 = myQueue2.enqueue(10)
  myQueue2 = myQueue2.enqueue(20)
  myQueue2 = myQueue2.enqueue(40)

  myQueue = myQueue2
  println(myQueue.first())

  var objs: Array[String] = Array.fill(3)("DEF")
  println(objs.toList)
  var strings: Array[String] = Array.fill(3)("ABC")
  println(strings.toList)

  copy(strings, objs)
  
  println(objs.toList)




