
/*
  Poprawność rozwiązania należy testować (z poziomu SBT) poleceniem:
  testOnly Test1
*/

def wystąpienia(arg: List[Int]): List[(Int,Int)] = {
  @annotation.tailrec
  def loop(arg: List[Int], acc: List[(Int,Int)]=List()): List[(Int,Int)] = arg match{
    case Nil => acc
    case head :: tail => loop(tail, addToEndOfLIstRecurencially(acc, (head, lengthRecurencially(tail.filter(_ == head)) + 1)))
  }
  loop(arg)
}

def addToEndOfLIstRecurencially[A](list: List[A], elem: A): List[A] = {
  list match {
    case Nil => List(elem)
    case head :: tail => head :: addToEndOfLIstRecurencially(tail, elem)
  }
}

def lengthRecurencially[A](list: List[A]): Int = {
  list match {
    case Nil => 0
    case head :: tail => 1 + lengthRecurencially(tail)
  }
}



@main def zad_1: Unit = {
  // „program główny” ma znaczenie czysto pomocnicze
  val arg = List(1,2,3,3,2,1)
  val arg2 = List(2,1,1,5)
  val wyn = wystąpienia(arg2)
  println(wyn)
}
