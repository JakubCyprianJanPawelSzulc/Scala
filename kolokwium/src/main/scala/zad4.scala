/*
    Korzystając wyłącznie z mechanizmów kolekcji języka Scala znajdź kraj o najdłużej rosnącym wskaźniku LadderScore.
    Innymi słowy, korzystając z załączonych danych szukamy kraju, dla którego wskaźnik LadderScore najdłużej
    utrzymał „dobrą passę” (z roku na rok się zwiększał).
    Zwróć uwagę na to, że w danych mogą wystąpić „linie” z brakującymi danymi. Takie linie powinny zostać
    POMINIĘTE. Brakujące dane oznaczają, że w linii występują sekwencje postaci: ,,, przykładowo:
        Kosovo,2020,6.294,,0.792,,0.880,,0.910,0.726,0.201
    Linie takie, jako „niewiarygodne” należy pominąć (oczywiście nie zmieniając samego pliku danych)
    zanim program rozpocznie analizę.
    W rozwiązaniu nie wolno używać zmiennych, ani konstrukcji imperatywnych, takich jak pętle
*/

def longestIncreasingSubsequence(lst: List[Double]): List[Double] = {
  lst.foldLeft(List.empty[Double]) {
    case (Nil, el) => List(el)
    case (acc @ (h :: _), el) => if (el > h) el :: acc else acc
  }.reverse
}

def jacek(data: List[String]): Any ={
    val data1 = data.map(_.split(",").toList)
    val data2 = data1.groupBy(el=>el(0))
    val data3 = data2.map(el=>(el._1, el._2.map(el=>el(2).toDouble)))
    val data4 = data3.map{case (country, scores) => (country, longestIncreasingSubsequence(scores))}
    val data5 = data4.map{case (country, scores) => (country, scores.length)}
    val data6 = data5.maxBy(_._2)
    data6
}

@main
def zad4(): Unit ={
    val data = io.Source.fromFile("C:/Users/JSadr/Desktop/studia/semestr4/scala/kolokwium/src/main/resources/world-happiness-report.csv").getLines().toList
    println(jacek(data))
}