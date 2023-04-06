package lab07

def przestaw[A](l: List[A]): List[A] = {
    l.grouped(2).flatMap(el => el.reverse).toList
}

@main def zadanie_04: Unit = {
    println(przestaw(List(1, 2, 3, 4, 5)))
}
