package lab06

def difficult[A](list: List[A])(len: Int, shift: Int = 1): List[List[A]] = {
  @annotation.tailrec
  def loop(remaining: List[A], acc: List[List[A]]): List[List[A]] = {
    remaining match {
      case Nil => acc.reverse
      case x if x.length < len => acc.reverse
      case _ =>
        val (group, rest) = remaining.splitAt(len)
        loop(remaining.drop(shift), group :: acc)
    }
  }
  loop(list, Nil)
}

@main def zadanie_03: Unit = {
  println("Testujemy zadanie 3")
  println(difficult(List(1, 2, 3, 4, 5))(3, 1))
}
