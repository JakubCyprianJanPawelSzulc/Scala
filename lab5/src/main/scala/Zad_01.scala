package lab05

import scala.annotation.tailrec

object Task01 {
  def isOrdered[A](leq: (A, A) => Boolean)(l: List[A]): Boolean = {
    @tailrec
    def loop(prev: A, rest: List[A]): Boolean = rest match {
      case Nil => true
      case head :: tail => leq(prev, head) && loop(head, tail)
    }

    l match {
      case Nil => true
      case head :: tail => loop(head, tail)
    }
  }

  def main(args: Array[String]): Unit = {
    val lt = (m: Int, n: Int) => m < n
    val lte = (m: Int, n: Int) => m <= n
    val lista = List(1, 2, 2, 5)
    val lista2 = List(2, 1, 1, 5)
    println(isOrdered(lt)(lista))
    println(isOrdered(lte)(lista))
    println(isOrdered(lt)(lista2))
    println(isOrdered(lte)(lista2))
  }
}