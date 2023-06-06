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
  def receive: Receive = {
    case Grupa.Runda => {
      // log.info("[grupa] Runda")
      zawodnicy.foreach(z => z ! Zawodnik.Próba)
    }
    case Grupa.Wynik(ocena) =>{
      context.become(zmiana(Map(sender() -> ocena)))
    }
    def zmiana(wyniki: Map[ActorRef, Option[Ocena]]) : Receive ={
      case Grupa.Runda => {
        // log.info("[grupa] Runda")
        zawodnicy.foreach(z => z ! Zawodnik.Próba)
      }
      case Grupa.Wynik(ocena) =>{
        if (wyniki.size + 1 == zawodnicy.size) {
          context.parent ! Organizator.Wyniki(wyniki + (sender() -> ocena))
        }
        context.become(zmiana(wyniki + (sender() -> ocena)))
      }
    }
  }
}
