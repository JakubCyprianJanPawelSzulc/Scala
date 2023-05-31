package jp1.akka.lab13.model

case class Ocena(nota1: Int, nota2: Int, nota3: Int)

/*
  o1 > o2 jeśli:
    - suma1 > suma2  (suma = nota1 + nota3 + nota3)
    - suma1 == suma2 oraz o1.nota1 > o2.nota1
    - suma1 == suma2, o1.nota1 == o2.nota1 oraz o1.nota3 > o2.nota3

  Jeśli o1 i o2 mają identyczne wszystkie noty to mamy REMIS

 */
