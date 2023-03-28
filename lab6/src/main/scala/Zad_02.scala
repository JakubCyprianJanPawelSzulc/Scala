package lab06

def freqMax[A](list: List[A]): (Set[A],Int) = {
  val freqMap = list.groupBy(identity).view.mapValues(_.size).toMap
  (freqMap.collect{ case (elem, freq) if freq == freqMap.values.maxOption.getOrElse(0) => elem }.toSet, freqMap.values.maxOption.getOrElse(0))
}

@main def zadanie_02: Unit = {
  println("Testujemy zadanie 2")
  println(freqMax(List(1, 1, 2, 4, 4, 3, 4, 1, 3)))
}
