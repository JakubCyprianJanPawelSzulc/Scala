package kolokwium_2

import akka.actor.{Actor, ActorLogging, Props, ActorRef, ActorSystem}

abstract class DoPracownika
case class Wstaw(słowo: String) extends DoPracownika

class Pracownik extends Actor with ActorLogging {
  def receive: Receive = {
    case Wstaw(słowo) => {
      if (słowo.length-1 == 0) {
        context.become(zliczanie(1))
      } else {
        val pierwszaLitera = słowo.charAt(1)
        val utf8String = pierwszaLitera.toInt.toString
        val pracownik = context.actorOf(Props[Pracownik](), utf8String)
        pracownik ! Wstaw(słowo.substring(1))
        context.become(zPracownikami(0, Array(pracownik)))
      }
    }
  }

  def zliczanie(licznik: Int): Receive = {
    case Wstaw(słowo) => {
      if (słowo.length-1 == 0) {
        context.become(zliczanie(licznik+1))
      } else {
        val pierwszaLitera = słowo.charAt(1)
        val utf8String = pierwszaLitera.toInt.toString
        val pracownik = context.actorOf(Props[Pracownik](), utf8String)
        pracownik ! Wstaw(słowo.substring(1))
        context.become(zPracownikami(licznik, Array(pracownik)))
      }
    }
    case I(słowo) => {
      sender() ! Ile(słowo, licznik)
    }
  }

  def zPracownikami(licznik: Int, pracownicy: Array[ActorRef]): Receive = {
    case Wstaw(słowo) => {
      if (słowo.length-1 == 0) {
        context.become(zPracownikami(licznik+1, pracownicy))
      } else {
        val pierwszaLitera = słowo.charAt(1)
        val utf8String = pierwszaLitera.toInt.toString
        if (pracownicy.exists(_.path.name == utf8String)) {
          pracownicy.find(_.path.name == utf8String).get ! Wstaw(słowo.substring(1))
        } else {
          val pracownik = context.actorOf(Props[Pracownik](), utf8String)
          pracownik ! Wstaw(słowo.substring(1))
          context.become(zPracownikami(licznik+1, pracownicy :+ pracownik))
        }
      }
    }
    case I(słowo) => {
      if (słowo.length-1 == 0) {
        sender() ! Ile(słowo, licznik)
      } else {
        val pierwszaLitera = słowo.charAt(1)
        val utf8String = pierwszaLitera.toInt.toString
        if (pracownicy.exists(_.path.name == utf8String)) {
          pracownicy.find(_.path.name == utf8String).get ! I(słowo.substring(1))
        } else {
          println(0)
        }
      }
    }
    case Ile(słowo, n) =>{
      val litera = self.path.name.toInt.toChar
      context.parent ! Ile(słowo + litera, n)
    }
  }
}
