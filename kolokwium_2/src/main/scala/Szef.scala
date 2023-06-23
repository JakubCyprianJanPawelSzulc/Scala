package kolokwium_2

import akka.actor.{Actor, ActorLogging, Props, ActorRef, ActorSystem}
import java.nio.charset.StandardCharsets

abstract class DoSzefa
case class W(słowo: String) extends DoSzefa
case class I(słowo: String) extends DoSzefa
case class Ile(słowo: String, n: Int) extends DoSzefa

class Szef extends Actor with ActorLogging {
  def receive: Receive = {
    case W(słowo) => {
      val pierwszaLitera = słowo.charAt(0)
      val utf8String = pierwszaLitera.toInt.toString
      val pracownik = context.actorOf(Props[Pracownik](), utf8String)
      pracownik ! Wstaw(słowo)
      context.become(zPracownikami(Array(pracownik)))
    }

  }
  def zPracownikami(pracownicy: Array[ActorRef]): Receive = {
    case W(słowo) => {
      val pierwszaLitera = słowo.charAt(0)
      val utf8String = pierwszaLitera.toInt.toString
      if (pracownicy.exists(_.path.name == utf8String)) {
        pracownicy.find(_.path.name == utf8String).get ! Wstaw(słowo)
      } else {
        val pracownik = context.actorOf(Props[Pracownik](), utf8String)
        pracownik ! Wstaw(słowo)
        context.become(zPracownikami(pracownicy :+ pracownik))
      }
    }
    case I(słowo) => {
      val pierwszaLitera = słowo.charAt(0)
      val utf8String = pierwszaLitera.toInt.toString
      if (pracownicy.exists(_.path.name == utf8String)) {
        pracownicy.find(_.path.name == utf8String).get ! I(słowo)
      } else {
        println(0)
      }
    }
    case Ile(słowo, n) =>{
      println(słowo.reverse +" : "+ n)
    }
  }
}

