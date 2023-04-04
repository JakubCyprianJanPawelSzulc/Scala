package lab07

def usun[A](lista: List[A], k: Int): List[A] = {
  lista.zipWithIndex.filter(el=>el(1)!=k).map(el=>el(0))
}

@main def zadanie_01: Unit = {
  println(usun(List(1,2,3,4,5),3))
}
