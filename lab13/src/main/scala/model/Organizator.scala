package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, ActorLogging, Props}

val akkaPathAllowedChars = ('a' to 'z').toSet union
  ('A' to 'Z').toSet union
  "-_.*$+:@&=,!~';.)".toSet

object Organizator {
  case object Start
  // rozpoczynamy zawody – losujemy 50 osób, tworzymy z nich zawodników
  // i grupę eliminacyjną
  case object Runda
  // polecenie rozegrania rundy (kwalifikacyjnej bądź finałowej) –  wysyłamy Grupa.Runda
  // do aktualnej grupy
  case object Wyniki
  // polecenie wyświetlenia klasyfikacji dla aktualnej grupy
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  // wyniki zwracane przez Grupę
  case object Stop
  // kończymy działanie
}

class Organizator extends Actor with ActorLogging {
  // importujemy komunikaty na które ma reagować Organizator
  import Organizator._

  def receive: Receive = {
    case Start =>
      // tworzenie 50. osób, opdowiadających im Zawodników
      // oraz Grupy eliminacyjnej
      val zawodnicy = List.fill(50) {
        val o = Utl.osoba()
        context.actorOf(Props(Zawodnik(o)), s"${o.imie}-${o.nazwisko}" filter akkaPathAllowedChars)
      }
      context.become(zZawodnikami(zawodnicy))
      // ...

    // Obsługa pozostałych komunikatów

  }
  def zZawodnikami(zawodnicy: List[ActorRef]): Receive = {
    case Runda =>
      val grupa = context.actorOf(Props(Grupa(zawodnicy)), "grupa")
      grupa ! Grupa.Runda
    case Organizator.Wyniki(w: Map[ActorRef, Option[Ocena]]) =>
      val sum = w.collect {
        case (actorRef, Some(ocena)) => (actorRef, ocena.nota1 + ocena.nota2 + ocena.nota3)
      }
      val sortedResults = sum.toList.sortBy(-_._2)
      val bestResults = sortedResults.take(20)
      context.become(zWynikami(bestResults))
    case Stop =>
      log.info("kończymy zawody w stanie zZawodnikami")
      context.system.terminate()
  }

  def zWynikami(wyniki: List[(ActorRef, Int)]): Receive = {
    case Runda =>
      log.info("tworzę grupę finałową")
      val grupa = context.actorOf(Props(Grupa(wyniki.map(_._1))), "grupa2")
      grupa ! Grupa.Runda
    case Organizator.Wyniki(w: Map[ActorRef, Option[Ocena]]) =>
      log.info("Wyniki koncowe przyszly")
      val sum = w.collect {
        case (actorRef, Some(ocena)) => (actorRef, ocena.nota1 + ocena.nota2 + ocena.nota3)
      }
      val sortedResults = sum.toList.sortBy(-_._2)
      val bestResults = sortedResults.take(20)
      context.become(zWynikamiKoncowymi(bestResults))
    case Organizator.Wyniki =>
      log.info("Wyniki po 1 rundzie:")
      wyniki.foreach(w => log.info(s"${w._1.path.name}: ${w._2}"))
    case Stop =>
      log.info("kończymy zawody w stanie zWynikami")
      context.system.terminate()
  }

  def zWynikamiKoncowymi(wyniki: List[(ActorRef, Int)]): Receive = {
    case Organizator.Wyniki =>
      log.info("Wyniki koncowe:")
      wyniki.foreach(w => log.info(s"${w._1.path.name}: ${w._2}"))
      context.become(koniec)
    case Stop =>
      log.info("kończymy zawody w stanie zWynikamiKoncowymi")
      context.system.terminate()
}
  def koniec: Receive = {
    case Stop =>
      log.info("kończymy zawody w stanie koniec")
      context.system.terminate()
  }

}
