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
      log.info("Grupa eliminacyjna")
      grupa ! Grupa.Runda
    case Organizator.Wyniki(w: Map[ActorRef, Option[Ocena]]) =>
      val sum = w.collect {
        case (actorRef, Some(ocena)) => (actorRef, ocena.nota1 + ocena.nota2 + ocena.nota3, ocena.nota1, ocena.nota2, ocena.nota3)
      }
      val sorted = sum.toList.sortBy(_._2).reverse
      val bestResults = sorted.take(20)
      context.become(zWynikami(bestResults))
    case Stop =>
      log.info("Kończymy zawody w stanie zZawodnikami")
      context.system.terminate()
    }

  def zWynikami(wyniki: List[(ActorRef, Int, Int, Int, Int)]): Receive = {
    case Runda =>
      log.info("Grupa finałowa")
      val grupa = context.actorOf(Props(Grupa(wyniki.map(_._1))), "grupa2")
      grupa ! Grupa.Runda

    case Organizator.Wyniki(w: Map[ActorRef, Option[Ocena]]) =>
      val sum = w.collect {
        case (actorRef, Some(ocena)) => (actorRef, ocena.nota1 + ocena.nota2 + ocena.nota3 + wyniki.find(_._1 == actorRef).get._2 + wyniki.find(_._1 == actorRef).get._3 + wyniki.find(_._1 == actorRef).get._4, ocena.nota1+wyniki.find(_._1 == actorRef).get._2, ocena.nota2+wyniki.find(_._1 == actorRef).get._3, ocena.nota3+wyniki.find(_._1 == actorRef).get._4)
      }
      val sorted = sum.toList.sortBy(_._2).reverse
      context.become(zWynikamiKoncowymi(sorted))
    case Wyniki =>
      log.info("Klasyfikacja:")
      wyniki.foreach {
        case (actorRef, suma, n1, n2, n3) =>
          log.info(s"${actorRef.path.name} $suma ($n1, $n2, $n3)")
      }
    case Stop =>
      log.info("Kończymy zawody w stanie zWynikami")
      context.system.terminate()
  }

  def zWynikamiKoncowymi(wyniki: List[(ActorRef, Int, Int, Int, Int)]): Receive = {
    case Wyniki =>
      log.info("Klasyfikacja:")
      wyniki.foreach {
        case (actorRef, suma, n1, n2, n3) =>
          log.info(s"${actorRef.path.name} $suma ($n1, $n2, $n3)")
      }
    case Stop =>
      log.info("Kończymy zawody w stanie zWynikamiKoncowymi")
      context.system.terminate()
  }
  
}


