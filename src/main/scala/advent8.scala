object advent8 extends App {

  import processText.mapFromName

  val splitted = mapFromName("src/data/displays.txt")(a => a.split("\\| "))

  val (inputs, outputs) = splitted.map(a => (a(0).split(" "), a(1).split(" "))).toList.unzip



  def num1478(output: List[Array[String]]) = {
    val groupedMap = output.flatten.groupBy(_.length).map{case (a, b) => (a, b.size)}
    // 1 is 2, 4 is 4, 7 is 3, 8 is 7
    // so  2+4+3+7
    groupedMap.collect{ case (a, b) => if (a== 2 | a == 4 | a == 3 | a == 7) b else 0}.sum
  }

  def processLength5(length5: List[String]) = {
    val splat = length5.appended(length5.head).sliding(2).toList map {
      case List(a, b) => (a, b, a.diff(b))
    }
    val (twoFive, others) = splat.partition(_._3.length == 2)
    val three = others.flatMap(a => List(a._1, a._2)).groupBy(identity).filter(_._2.size == 2).keys.head
    (twoFive.map(a => (a._1, a._2)).head, three)
  }


  def processLength6(length6: List[String], twoFive: (String, String), mappedRest: Map[Int, Set[Char]]) = {
    val setted = length6.map(_.toSet)
    val (five, two) = twoFive match {
      case (a, b) => if (setted.count(set => a.toSet.subsetOf(set)) > 0) (a, b) else (b, a)
    }
    val (sixNine, zeroList) = setted.partition(set => five.toSet.subsetOf(set))
    // 9 contains 4
    val (nineList, sixList) = sixNine.partition(set => mappedRest.getOrElse(4, Set()).subsetOf(set))
    Map(
      0 -> zeroList.head,
      2 -> two.toSet,
      5 -> five.toSet,
      6 -> sixList.head,
      9 -> nineList.head
    ).++(mappedRest)
  }

  def deduce(inputLine: Array[String], outputLine: Array[String]) = {
    // 1 -> 2, 4 -> 4, 7 -> 3, 8 -> 7
    // 0 -> 6, 2 -> 5, 3 -> 5, 5 -> 5, 6 -> 6, 9 -> 6
    // 2,3 differ by 1, 3,5 differ by 1, 2,5 differ by 2, 5 contains a letter than 2,3 do not
    // 9,6 contain the 5, 0 doesnt
    val groupedMap = inputLine.toList.groupBy(_.length)
    val (twoFive, three) = processLength5(groupedMap.getOrElse(5, List()))
    val restMap = groupedMap.filter(a => a._1 == 2 | a._1 == 4 | a._1 == 3 | a._1 == 7).map {
      case (length, xs) =>
        if (length == 2) 1 -> xs.head.toSet
        else if (length == 4) 4 -> xs.head.toSet
        else if (length == 3) 7 -> xs.head.toSet
        else 8 -> xs.head.toSet
    } ++ Map(3 -> three.toSet)
    val finalMap = processLength6(groupedMap.getOrElse(6, List()), twoFive, restMap).map(_.swap)
    outputLine.map(a => finalMap(a.toSet).toString).mkString.toInt
  }

  def deduceMultiple(inputLines: List[Array[String]], outputLines: List[Array[String]]) = {
    inputLines.zip(outputLines).map{case (a, b) => deduce(a, b)}.sum
  }

//  println(deduce(Array("afebd", "ecdgfb", "gacfed", "dgaeb", "bf", "acefd", "fgdabec", "bfd", "bedcaf", "bafc"), Array("afcbed", "fb", "bfd", "bdf")))

//  println(num1478(outputs))

  println(deduceMultiple(inputs, outputs))
}
