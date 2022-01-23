package Lista12

import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

object Zad2a {
  def pairFut[A, B](fut1: Future[A], fut2: Future[B]): Future[(A, B)] =
    fut1 zip fut2

  def main(args: Array[String]): Unit = {
    val pair0 = pairFut(Future{1}, Future{2})
    val pair1 = pairFut(Future{1 + 2 + 3}, Future{"2"})
    val pair2 = pairFut(Future{1 + 2 + 3}, Future{"ABC" + "BCD"})
    val pair3 = pairFut(Future{}, Future{})

    println(pair0.isCompleted)
    pair0 foreach println
    pair1 foreach println
    pair2 foreach println
    pair3 foreach println

  }
}
