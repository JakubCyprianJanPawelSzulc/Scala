package lab05

def chessboard: String = {
  val rows = List("8", "7", "6", "5", "4", "3", "2", "1")
  val columns = List("a", "b", "c", "d", "e", "f", "g", "h")
  val positions = rows.map(row => columns.map(column => (row, column)))
  val flattened = positions.flatMap(row => row)
  flattened.map(pair=>{
    if (pair._2=="h") s"(${pair._1}, ${pair._2})\n"
    else s"(${pair._1}, ${pair._2}) "
  }).mkString("")
}

@main def zadanie_03: Unit = {
  // Program powinien umożliwić „sprawdzenie” działania
  // funkcji „szachownica”.
  println(chessboard)
}
