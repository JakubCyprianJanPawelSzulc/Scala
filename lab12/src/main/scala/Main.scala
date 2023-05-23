import akka.actor.{ActorSystem, Actor, ActorLogging, ActorRef, Props}

case class Oblicz(n: Int)
case class Wynik(n: Int, fib: BigInt)


class Boss extends Actor with ActorLogging {
    var nadzorca : ActorRef = _
    def receive : Receive ={
        case Oblicz(liczba) => {
            if(context.child("nadzorca").isEmpty){
                nadzorca = context.actorOf(Props(new Nadzorca()), "nadzorca")
            }
            nadzorca ! Oblicz(liczba)
        }
        case Wynik(który, elem) => {
            log.info(s"Element ${który} jest rowny ${elem}")
        }
    }
}
var pracownik : ActorRef = _
class Nadzorca(var cache: Map[Int, BigInt] = Map(1 -> 1, 2 -> 1), doZrobienia: Set[Int] = Set()) extends Actor with ActorLogging {
    def receive : Receive = {
        case Oblicz(liczba) => {
            if(cache.contains(liczba)){
                log.info(s"Element ${liczba} zostal juz kiedys obliczony")
                context.parent ! Wynik(liczba, cache(liczba))
            }else{
                if(context.child("pracownik").isEmpty){
                    pracownik = context.actorOf(Props(new Pracownik(liczba)), "pracownik")
                }
                pracownik ! Oblicz(liczba)
            }
        }
        case Wynik(który, liczba) => {
            val nowaMapa1 = cache + (który-> liczba)
            cache = nowaMapa1
            println(cache)
            context.parent ! Wynik(który, liczba)
        }

    }

}

class Pracownik(k: Int) extends Actor with ActorLogging {
    def receive : Receive = {
        case Oblicz(elem) =>{
            def fibonacciRecursive(n: Int): Int = {
                if(n<1) 0
                else if (n == 1) 1
                else fibonacciRecursive(n - 1) + fibonacciRecursive(n - 2)
            }
            context.parent ! Wynik(elem,fibonacciRecursive(elem))
        }
    }
}

@main def zadaniećwiczenia : Unit = {
    val system = ActorSystem("Fibonacci")
    val szef = system.actorOf(Props(new Boss()), "szef")
    szef ! Oblicz(9)
    szef ! Oblicz(10)
    Thread.sleep(2000)
    szef ! Oblicz(9)
}