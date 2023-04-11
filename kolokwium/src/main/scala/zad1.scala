def countResults[A,B,C](l1: List[A], l2: List[B])(f: (A, B) => C): Set[(C, Int)] = {
  @annotation.tailrec
  def loop(arr1: List[A], arr2: List[B], acc: Map[C, Int] = Map())(func: (A, B) => C): Set[(C, Int)] = (arr1,arr2) match {
    case (Nil, _) | (_, Nil) => acc.toSet
    case (h1 :: Nil, h2 :: Nil) =>
      val result = f(h1, h2)
      acc.get(result) match {
        case Some(count) => loop(Nil, Nil, acc.updated(result, count + 1))(f)
        case None => loop(Nil, Nil, acc + (result -> 1))(f)
      }
    case (h1 :: t1, h2 :: t2) =>
      val result = f(h1, h2)
      acc.get(result) match {
        case Some(count) => loop(t1, t2, acc.updated(result, count + 1))(f)
        case None => loop(t1, t2, acc + (result -> 1))(f)
      }
  }
  loop(l1, l2)(f)
}

@main
def zad1(): Unit = {
  println(countResults(List(1,2,3), List(4,5,4,6))(_+_))
}
