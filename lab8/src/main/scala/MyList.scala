package lab09

sealed trait MyList[+A]
case object Empty extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {

  def head[A](list: MyList[A]): A = list match {
    case Cons(head, tail) => head
    case _ => throw IllegalArgumentException("Head of an empty MyList")
  }

  // wynik: MyList-a zawierająca wszystkie elementy poza pierwszym
  def tail[A](list: MyList[A]): MyList[A] = {
    list match {
      case Cons(head, tail) => tail
      case _ => throw IllegalArgumentException("Tail of an empty MyList")
    }
  }

  // wynik: długość MyList-y będącej argumentem
  def length[A](list: MyList[A]): Int = {
    def loop(l: MyList[A], acc: Int): Int = {
      l match {
        case Cons(h, tl) => loop(tl, acc + 1)
        case _ => acc
      }
    }
    loop(list, 0)
  }

  // wynik: MyList-a zawierająca wszystkie elementy poz n początkowymi
  def drop[A](list: MyList[A], n: Int): MyList[A] = {
    def loop(l: MyList[A], acc: Int): MyList[A] = {
      l match {
        case Cons(h, tl) if acc < n => loop(tl, acc + 1)
        case _ => l
      }
    }
    loop(list, 0)
  }

  // wynik: „odwrócony” argument
  def reverse[A](list: MyList[A]): MyList[A] = {
    def loop(l: MyList[A], acc: MyList[A]): MyList[A] = {
      l match {
        case Cons(h, tl) => loop(tl, Cons(h, acc))
        case _ => acc
      }
    }
    loop(list, Empty)
  }

  // wynik: argument po odrzuceniu początkowych elementów spełniających p
  def dropWhile[A](l: MyList[A])(p: A => Boolean): MyList[A] = {
    def loop(l: MyList[A], acc: MyList[A]): MyList[A] = {
      l match {
        case Cons(h, tl) if p(h) => loop(tl, acc)
        case _ => acc
      }
    }
    loop(l, Empty)
  }

  // wynik: połączone MyList-y list1 oraz list2
  def append[A](list1: MyList[A], list2: MyList[A]): MyList[A] = {
    def loop(l: MyList[A], acc: MyList[A]): MyList[A] = {
      l match {
        case Cons(h, tl) => loop(tl, Cons(h, acc))
        case _ => acc
      }
    }
    loop(list1, list2)
  }

  // wynik: MyList-a składająca się ze wszystkich alementów argumentu, poza ostatnim
  def allButLast[A](list: MyList[A]): MyList[A] = {
    def loop(l: MyList[A], acc: MyList[A]): MyList[A] = {
      l match {
        case Cons(h, tl) if tl != Empty => loop(tl, Cons(h, acc))
        case _ => acc
      }
    }
    loop(list, Empty)
  }

}

@main def listy: Unit = {
  val l1 = Cons(1, Cons(2, Cons(3, Empty)))
  val l2 = Cons(4, Cons(5, Cons(6, Empty)))

  val res = MyList.head(l1)
  println(s"MyList.head($l1) == $res")
  println(s"MyList.tail($l1) == ${MyList.tail(l1)}")
  println(s"MyList.length($l1) == ${MyList.length(l1)}")
  println(s"MyList.drop($l1, 2) == ${MyList.drop(l1, 2)}")
  println(s"MyList.reverse($l1) == ${MyList.reverse(l1)}")
  println(s"MyList.dropWhile($l1)(x => x < 3) == ${MyList.dropWhile(l1)(x => x < 3)}")
  println(s"MyList.append($l1, $l2) == ${MyList.append(l1, l2)}")
  println(s"MyList.allButLast($l1) == ${MyList.allButLast(l1)}")
  // println(MyList.head(Empty)) // spowoduje „wyjątek”
}
