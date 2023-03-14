package lab04

def tasuj(l1: List[Int], l2: List[Int]): List[Int] = {
  @annotation.tailrec
  def helper(l1: List[Int], l2: List[Int], acc: List[Int] = List()): List[Int] = {
    if (l1.isEmpty && l2.isEmpty) return acc

    if (l2.isEmpty || (l1.nonEmpty && l1.head <= l2.head)) {
      if (acc.nonEmpty && l1.head == acc.last) helper(l1.tail, l2, acc)
      else helper(l1.tail, l2, acc.appended(l1.head))
    } else {
      if (acc.nonEmpty && l2.head == acc.last) helper(l1, l2.tail, acc)
      else helper(l1, l2.tail, acc.appended(l2.head))
    }
  }

  helper(l1, l2)
}

@main def zadanie_02: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „tasuj”.
  println(tasuj(List(2, 4, 3, 5), List(1, 2, 2, 3, 1, 5))) // List(1, 2, 3, 1, 4, 3, 5)
}
