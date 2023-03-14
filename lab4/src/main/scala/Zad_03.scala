package lab04

def sumuj(l: List[Option[Double]]): Option[Double] = {
  @annotation.tailrec
  def loop(l: List[Option[Double]], acc: Double): Option[Double] = l match {
    case Nil => if (acc > 0) Some(acc) else None
    case None :: tail => loop(tail, acc)
    case Some(d) :: tail => loop(tail, if (d > 0) acc + d else acc)
  }
  loop(l, 0)
}

@main def zadanie_03: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „sumuj”.
  val lista = List(Some(4.0), Some(-3.0), None, Some(1.0), Some(0.0))
  println(sumuj(lista))
}
