//==========================================================================
// Metoda porównuje napisy zgodnie z polskim porządkiem alfabetycznym
// Jedyna zmiana jaka może być tutaj potrzebna to „zamiana komentarzy”
// w linijkach 9 oraz 10.
//--------------------------------------------------------------------------
def ltePL(s1: String, s2: String) = {
  import java.util.Locale
  import java.text.Collator
  val locale = new Locale("pl", "PL") // dla starszych wersji JRE/JDK
  // val locale = Locale.of("pl", "PL") // dla nowszych wersji JRE/JDK
  Collator.getInstance(locale).compare(s1, s2) <= 0
}

// Metoda nie wymaga zmian. Wczytuje dane z pliku i zwraca listę linii
def dane: List[String] = {
  import scala.io.Source
  val plik = Source.fromFile("src/main/resources/machiavelli.txt", "UTF-8")
  plik.getLines.toList
}
//==========================================================================

// Jedyna rzecz do zaimplementowania, czyli metoda „wynik”:
def wynik: List[(String, List[Int])] = {
// def wynik: Any = {
  val linesIndexed = dane.map(_.split("\n").toList).zipWithIndex
  val linesIndexed2 = linesIndexed.map(el=>(el(0), el(1)+1))
  val uniqueWords = dane.map(_.split("\\s+").toList).flatten.groupBy(x => x.toLowerCase.filter(c=>c.isLetter)).map(elem => (elem._1))
  // val linesWithWords = linesIndexed.map(x=>(x._1(0).split("\\s+").toList, x._2))
  // val linesWithWords2 = linesWithWords.map(x=>(x._1.map(el=>(el.toLowerCase.filter(c=>c.isLetter), x._2))))
  val wordsWithLines = uniqueWords.map(elem => (elem, linesIndexed2.filter(x => x._1(0).toLowerCase().contains(elem)).map(x => x._2)))
  // val wordsWithLines = uniqueWords.map(elem => (elem, linesWithWords2.filter(x=> x._1.contains(elem)).map(x=>x._2)))
  val wordsWithUniqueLinesSorted = wordsWithLines.map(elem => (elem._1, elem._2.distinct.sorted))
  wordsWithUniqueLinesSorted.toList.sortBy(el=>el._1)
  // linesWithWords2
}

/*
  Poprawność rozwiązania należy testować (z poziomu SBT) poleceniem:
  testOnly Test2
*/

@main def zad_2: Unit = {
  // „program główny” ma znaczenie czysto pomocnicze
  // if ltePL("a", "ą") then println("OK")
  // else println("to nie powinno się zdarzyć…")
  println(wynik)

}
