/*
  Zadanie 1. Stwórz definicję funkcji

  def znajdź[A,B](alist: List[A], blist: List[B])(pred: Pred[(A,B)]): Set[(A,B)] = { ... }

  tak, aby jej wynikiem był zbiór wszystkich par (a,b) takich, że a i b są dowolnymi elementami
  list „alist” i „blist” odpowiednio, dla których zachodzi pred(a,b).

  W rozwiązaniu skorzystaj z rekurencji ogonowej (pmiętaj o użyciu adnotacji
  „@annotation.tailrec”!) i dopasowania wzorców. Nie używaj żadnych operacji
  na kolekcjach poza „::” oraz „++” (dodawanie zbiorów).

  WARTOŚĆ ZADANIA: 2 pkt
*/

package zad1kolos2020

def znajdź[A,B](alist: List[A], blist: List[B])(pred: (A,B) => Boolean): Set[(A,B)] = {
  @annotation.tailrec
  def pomocnicza(list1: List[A], list2: List[B], acc: Set[(A,B)]=Set()): Set[(A,B)] = {
    (list1, list2) match {
      case (Nil, _) => acc
      case (h1::t1, h2::t2) => if (pred(h1,h2)) pomocnicza(h1::t1, t2, acc + ((h1,h2))) else pomocnicza(h1::t1, t2, acc)
      case (h1::t1, Nil) => pomocnicza(t1, blist, acc)

    }
  }
  pomocnicza(alist, blist)
}

@main
def zad1kolos2020 = {
  val a = List(1,2,3,4,5)
  val b = List(1,2,3,4,5)
  val c = List(1,2,3,4,5)
  val d = List(6,7,8,9,10)

  val pred = (x: Int, y: Int) => x == y
  val pred2 = (x: Int, y: Int) => x > y

  // println(znajdź(a,b)(pred))
  println(znajdź(c,d)(pred2))


}