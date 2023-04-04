package lab07

def indeksy[A](lista: List[A], el: A): Set[Int] = {
  lista.zipWithIndex.filter(elem=>elem(0)==el).map(el=>el(1)).toSet
}

@main def zadanie_02: Unit = {
    println(indeksy(List(3,1,2,3,4,3,5),3))
}
