package kolokwium_2

/*
  UWAGA: W sformułowaniu zadania zabrakło jednej wskazówki/informacji:

    Jeśli słowo, o którego liczbę wystąpień pytamy, nie występuje w strukturze
    hierarchii aktorów, to program powinien zwrócić dla niego (tzn. Szef powinien
    wypisać w konsoli) wartość 0.

    Przykładowo, w sytuacji zobrazowanej na rysunku, komunikaty I("al") oraz
    I("alert") powinny zwrócić (w konsoli) odpowiedź 0.
*/

import akka.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

// Metoda nie wymaga zmian. Wczytuje dane z pliku i zwraca listę słów
private def dane: List[String] = {
  import scala.io.Source
  val plik = Source.fromFile("src/main/resources/dane.txt", "UTF-8")
  plik
    .getLines.toList
    .flatMap { linia => linia.split("[^\\p{IsAlphabetic}]+").toList }
}

@main def main: Unit = {
  val system = ActorSystem("sys")
  val szef = system.actorOf(Props[Szef](), "szef")
  // println(dane)
  dane.foreach { słowo => szef ! W(słowo) }
  szef ! I("dniestru")
  szef ! I("jaja")
  szef ! I("nie")
}
