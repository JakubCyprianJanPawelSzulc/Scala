package lab05

def deStutter[A](list: List[A]): List[A] = {
    list.foldLeft(List[A]())((acc, elem) => {
      if (acc.isEmpty || acc.last != elem) acc :+ elem
      else acc
    })
}

@main def zadanie_02: Unit = {
  val l = List(1, 1, 2, 4, 4, 4, 1, 3)
  println(deStutter(l)) //(1, 2, 4, 1, 3)
}
