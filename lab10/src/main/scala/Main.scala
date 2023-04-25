package lab10

sealed trait Frm {
  def needsParentheses: Boolean
}
case object False extends Frm {
  override def toString: String = "False"
  override def needsParentheses: Boolean = false
}
case object True extends Frm {
  override def toString: String = "True"
  override def needsParentheses: Boolean = false
}
case class Not(f: Frm) extends Frm {
  override def toString: String = {
    val innerStr = f.toString
    if (f.needsParentheses) s"!($innerStr)"
    else s"!$innerStr"
  }
  override def needsParentheses: Boolean = f match {
    case _: And | _: Or => true
    case _ => false
  }
}
case class And(f1: Frm, f2: Frm) extends Frm {
  override def toString: String = {
    val leftStr = f1.toString
    val rightStr = f2.toString
    val leftParen = if (f1.needsParentheses) s"($leftStr)" else leftStr
    val rightParen = if (f2.needsParentheses) s"($rightStr)" else rightStr
    s"$leftParen & $rightParen"
  }
  override def needsParentheses: Boolean = true
}
case class Or(f1: Frm, f2: Frm) extends Frm {
  override def toString: String = {
    val leftStr = f1.toString
    val rightStr = f2.toString
    val leftParen = if (f1.needsParentheses) s"($leftStr)" else leftStr
    val rightParen = if (f2.needsParentheses) s"($rightStr)" else rightStr
    s"$leftParen | $rightParen"
  }
  override def needsParentheses: Boolean = true
}
case class Imp(f1: Frm, f2: Frm) extends Frm {
  override def toString: String = {
    val leftStr = f1.toString
    val rightStr = f2.toString
    val leftParen = if (f1.needsParentheses) s"($leftStr)" else leftStr
    val rightParen = if (f2.needsParentheses) s"($rightStr)" else rightStr
    s"$leftParen -> $rightParen"
  }
  override def needsParentheses: Boolean = true
}

val frm = Imp(Or(True, False), Not(And(True, False)))

// „UWAGA”: W rozwiązaniach poniższych „zadań” (tam gdzie to „możliwe”)
//        wykorzystaj rekurencję „ogonową”.

@main def zad_1: Unit = {
  // Ćwiczenie 1: zaimplementuj toString tak, aby „minimalizowało”
  //              liczbę nawiasów

  println(frm)
  // Powinno „wyprodukować” coś „w stylu”:
  // (True | False) -> !(True & False)
}

@main def zad_2: Unit = {
  // Ćwiczenie 2: zaimplementuj funkcję wyliczającą wartość logiczną
  //              formuły.
  def eval(f: Frm): Boolean = f match {
    case False => false
    case True => true
    case Not(f1) => !eval(f1)
    case And(f1, f2) => eval(f1) && eval(f2)
    case Or(f1, f2) => eval(f1) || eval(f2)
    case Imp(f1, f2) => !eval(f1) || eval(f2)
  }

  println(eval(frm))
}

@main def zad_3: Unit = {
  // Ćwiczenie 3: napisz funkcję wyliczającą dla danej formuły „f”
  //              zbiór jej wszystkich „podformuł”
  def closure(f: Frm): Set[Frm] = {
    def closureRec(f: Frm, acc: Set[Frm]): Set[Frm] = f match {
      case False | True => acc + f
      case Not(f1) => closureRec(f1, acc + f)
      case And(f1, f2) => closureRec(f1, closureRec(f2, acc + f))
      case Or(f1, f2) => closureRec(f1, closureRec(f2, acc + f))
      case Imp(f1, f2) => closureRec(f1, closureRec(f2, acc + f))
    }
    closureRec(f, Set.empty)
  }

  val frm = Imp(Or(True, False), Not(And(True, False)))
  println(closure(frm))
  //
  // np. dla formuły frm „powyżej” wynikiem powinien być zbiór:
  // 
  // { True, False, True | False, True & False, !(True & False), frm }
  //
  // Jak „widać”, closure(frm) „zawiera” również formułę będącą „argumentem”.
}