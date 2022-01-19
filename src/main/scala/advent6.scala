import scala.annotation.tailrec

object advent6 extends App {

  // Started with simulating each individually, but processing time grew with number of lanterns
  // Moved to hashmap from numberOfDaysLeft -> numberOfLanternsWithThatNumberOfDays

  case class Lantern(n: Int) {

    def simulateDay: (Lantern, Option[Lantern]) = {
      if (n == 0) (Lantern(6), Some(Lantern(8)))
      else (Lantern(n-1), None)
    }

    def simDayList: List[Lantern] = simulateDay match {
      case (l1, Some(l2)) => List(l1, l2)
      case (l1, None) => List(l1)
    }
  }

//  @tailrec
//  def simulateDays(school: List[Lantern], n: Int): List[Lantern] = {
//    if (n == 0) school
//    else simulateDays(school.flatMap(_.simDayList), n-1)
//  }
//
//  def schoolAfterN(school: List[Lantern], n: Int): Int = simulateDays(school, n).size

  def group(school: List[Lantern]): Map[Int, BigInt] = {
    (0 to 8).zip(List.fill(8)(BigInt(0))).toMap ++ {
      school.groupBy(_.n).map { case (a, b) => (a, BigInt(b.size)) }
    }
  }

  def processMapDay(schoolMap: Map[Int, BigInt]) = {
    val resetable = schoolMap.getOrElse(0,BigInt(0))
    val newMap = schoolMap.map {
      case (k, v) =>
        if (k == 0) (8, resetable)
        else (k-1, v)
    }
    newMap.updated(6, newMap.getOrElse(6, BigInt(0)) + resetable)
  }

  @tailrec
  def mapAfterN(schoolMap: Map[Int, BigInt], n: Int): BigInt = {
    if (n == 0) schoolSize(schoolMap)
    else mapAfterN(processMapDay(schoolMap), n-1)
  }


  def schoolSize(schoolMap: Map[Int, BigInt]): BigInt = schoolMap.foldRight(BigInt(0)){case ((_, d), b) =>d+b}

  def answer(n: Int, ints: Int*): BigInt = mapAfterN(group(ints.map(Lantern).toList), n)

  import processText.mapFromName

  val inputInts = mapFromName("src/data/lanterns.txt")(_.split(",")).flatten.map(_.toInt)

  println(
    answer(256, inputInts.toList:_*)
  )

}
