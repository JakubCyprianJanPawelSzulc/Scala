/*
  Używając rekurencji ogonowej zdefiniuj funkcję:
    def pairwiseTest[A](l: List[A])(pred: (A, A) => Boolean)
  która sprawdzi, czy predykat „pred" jest spełniony dla wszystkich par
  elementów listy „l” o indeksach (i, długość(l) - i), dla i = 0.. długość(l)/2.
  Przykładowo, dla listy List(1,2,3,4,3,2,1) oraz predykatu równości, sprowadzi
  się to do następującej serii weryfikacji równości:
    l(0) == l(6)
    l(1) == l(5)
    l(2) == l(4)
    l(3) == l(3)
  Ogólnie, seria taka będzie miała postać:
    pred(l(0), l(l.length-1)) == true
    pred(l(1), l(l.length-2)) == true
    pred(l(2), l(l.length-3)) == true
    ...
    pred(l(l.length/2), l(l.length - l.length/2 - 1)) == true
  W przypadku pustej listy funkcja powinna zwrócić true
  Uwaga: w rozwiązaniu nie używaj zmiennych, ani mechanizmów imperatywnych jak pętle.
  Nie używaj też kolekcji języka Scala.
*/

def pairwiseTest[A](l: List[A])(pred: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(arr: List[A], acc: Boolean = true)(pred: (A, A) => Boolean): Boolean = arr match{
        case Nil => acc
        case head :: Nil => loop(Nil, pred(head,head))(pred)
        case head +: Nil :+ last => if (pred(head, last))==true then loop(Nil, pred(head, last))(pred) else false
        case head +: body :+ last => if (pred(head,last))==true then loop(body, pred(head, last))(pred) else false
        case _ => acc
    }

    loop(l)(pred)
    
}

@main
def zad2: Unit = {
    val l = List(1,2,3,4,3,2,1)
    val l2 = List(1,2,3,4,3,2,2)

    println(pairwiseTest(l)(_ == _))
    println(pairwiseTest(l2)(_ == _))

}