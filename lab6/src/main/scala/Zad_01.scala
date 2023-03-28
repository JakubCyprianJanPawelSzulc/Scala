package lab06

def subseq[A](list: List[A], begIdx: Int, endIdx: Int): List[A] = {
  list.drop(begIdx).take(endIdx - begIdx + 1)
}

@main def zadanie_01: Unit = {
  println("Testujemy zadanie 1")
  println(subseq(List(1, 2, 3, 4, 5), 1, 3))
}
