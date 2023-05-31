package jp1.akka.lab13.model

import akka.actor.{Actor, ActorRef, ActorLogging}

object Grupa {
  case object Runda
  // Zawodnicy mają wykonać swoje próby – Grupa
  // kolejno (sekwencyjnie) informuje zawodników
  // o konieczności wykonania próby i „oczekuje”
  // na ich wynik (typu Option[Ocena])
  case object Wyniki
  // Polecenie zwrócenia aktualnego rankingu Grupy
  // Oczywiście klasyfikowani są jedynie Zawodnicy,
  // którzy pomyślnie ukończyli swoją próbę
  case class Wynik(ocena: Option[Ocena])
  // Informacja o wyniku Zawodnika (wysyłana przez Zawodnika do Grupy)
  // np. Wynik(Some(Ocena(10, 15, 14)))
  // Jeśli zawodnik nie ukończy próby zwracana jest wartość Wynik(None)
  case object Koniec
  // Grupa kończy rywalizację
}

class Grupa(zawodnicy: List[ActorRef]) extends Actor with ActorLogging {
  var wyniki: Map[ActorRef, Option[Ocena]] = Map()
  def receive: Receive = {
    case Grupa.Runda => {
      // log.info("[grupa] Runda")
      zawodnicy.foreach(z => z ! Zawodnik.Próba)
    }
    case Grupa.Wynik(ocena) =>{
      wyniki += (sender() -> ocena)
      // log.info(s"[grupa] otrzymałem wynik")
      if (wyniki.size == zawodnicy.size) {
        // log.info(s"[grupa] wszyscy zawodnicy zakończyli próby")
        context.parent ! Organizator.Wyniki(wyniki)
      }
    }
    // case msg => log.info(s"msg")
  }
}
