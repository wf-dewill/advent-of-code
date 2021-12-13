import scala.io.Source

object advent1 extends App {

  val trial = List(199,
  200,
  208,
  210,
  200,
  207,
  240,
  269,
  260,
  263)

  def howManyIncreased(measurements: List[Int]): Int =
    measurements.sliding(2).count {
      case List(a, b) => b > a
      case _ => false
    }

  assert(howManyIncreased(trial) == 7)

  val fileName = "" // File name goes here
  val source = Source.fromFile(fileName)
  println({
    val res = howManyIncreased(source.getLines().toList.map(_.toInt))
    source.close()
    res
  })


  val intLines: Iterator[Int] = source.getLines().map(_.toInt)

  def getSums(ints: List[Int]): List[Int] = ints.sliding(3).map(_.sum).toList

  println(
    howManyIncreased(getSums(intLines.toList))
  )
}
