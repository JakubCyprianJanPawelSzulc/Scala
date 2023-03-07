package lab03

def reverse(napis: String): String = {
  @annotation.tailrec
  def pomocnicza(remaining: String, reversed: String): String = {
    if (remaining.isEmpty) reversed
    else pomocnicza(remaining.tail, s"${remaining.head}$reversed")
  }
  pomocnicza(napis, "")
}

@main def zad_03(napis: String): Unit = {
  //Napisz funkcje reverse, która dla podanego napisu zwraca odwrócony napis
  //Wykorzystaj operacje head i tail na napisach
  val odwrócony = reverse(napis)
  println(s"$napis od konca to $odwrócony")
}
