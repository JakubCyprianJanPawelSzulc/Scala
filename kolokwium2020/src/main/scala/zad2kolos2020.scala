/*
  Zadanie 2. Korzystając z mechanizmów kolekcji języka Scala zdefiniuj funkcje

  def stwórzIndeks(): Map[String, List[Int]]

  def słowa(indeks: Map[String, List[Int]], nrLinii: Int): Set[String]

  Pierwsza z nich powinna utworzyć indeks wszystkich słów występujących w pliku
  „tekst.txt”. Każdemu słowu należy przypisać listę wszystkich numerów linii, w których
  ono występuje. W ramach takiej listy numery linii powinny być uporządkowane rosnąco
  nie mogą się powtarzać. Budując indeks traktuj wielkie i małe litery identycznie!

  Druga z funkcji, na bazie zbudowanego przez stwórzIndeks() indeksu (parametr indeks),
  powinna dla podanego numeru linii (parametr nrLinii) zwrócić zbiór wszystkich słów
  występujących w tej linii. UWAGA! Funkcja „słowa” musi korzystać z indeksu, a nie
  z pliku „tekst.txt”!

  Rozwiązując zadanie wykorzystaj JEDYNIE mechanizmy kolekcji – w szczególności nie
  definiuj żadnej funkcji rekurencyjnie!!!

  WARTOŚĆ ZADANIA: 3 pkt
*/

package zad2kolos2020
import scala.io.Source

def stwórzIndeks(): Map[String, List[Int]] = {
  val data = io.Source.fromFile("C:/Users/JSadr/Desktop/studia/semestr4/scala/kolokwium2020/src/main/scala/tekst.txt").getLines().toList
  val linesIndexed = data.map(_.split("\n").toList).zipWithIndex
  val uniqueWords = data.map(_.split(" ").toList).flatten.groupBy(x => x.toLowerCase.replaceAll("[,.!?;:\"']", "")).map(elem => (elem._1))
  val wordsWithLines = uniqueWords.map(elem => (elem, linesIndexed.filter(x => x._1(0).toLowerCase().contains(elem)).map(x => x._2)))
  val wordsWIthUniqueLinesSorted = wordsWithLines.map(elem => (elem._1, elem._2.distinct.sorted))
  wordsWIthUniqueLinesSorted.toMap
}

def słowa(indeks: Map[String, List[Int]], nrLinii: Int): Set[String] = {
  indeks.filter(x => x._2.contains(nrLinii)).map(x => x._1).toSet
}


@main
def zad2kolos2020()={
  // println(stwórzIndeks())
  println(słowa(stwórzIndeks(), 0))
}






