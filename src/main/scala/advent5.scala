import scala.io.Source

object advent5 extends App {

  case class Point(x: Int, y: Int)
  case class Line(point1: Point, point2: Point) {

    lazy val horizontal: Boolean = {
      (point1, point2) match {
        case (Point(x,y), Point(a, b)) => if (a == x) false else y == b
      }
    }

    lazy val vertical: Boolean = {
      (point1, point2) match {
        case (Point(x,y), Point(a, b)) => if (a == x) true else y == b
      }
    }

    lazy val xs: (Int, Int) = List(point1.x, point2.x) match {case List(a, b) => (a, b)}

    lazy val ys: (Int, Int) = List(point1.y, point2.y) match {case List(a, b) => (a, b)}

    def points: Seq[Point] = {

      val numX = if (xs._2 > xs._1) (xs._1 to xs._2) else (xs._1 to xs._2 by -1)
      val numY = if (ys._2 > ys._1) (ys._1 to ys._2) else (ys._1 to ys._2 by -1)
      if (numX.size == numY.size)
        for {
          (x, y) <- numX.zip(numY)
        } yield Point(x, y)
      else if (numX.size > numY.size)
        for {
          x <- numX
        } yield Point(x, ys._1)
      else
        for {
          y <- numY
        } yield Point(xs._1, y)
    }

  }

  object Line {
    def apply(elems: ((Int, Int), (Int, Int))): Line = {
       elems match {
        case ((a, b), (x, y)) => Line(Point(a, b), Point(x, y))
      }
    }

  }

  def processLine(line: String): Line = line.split(" -> ").flatMap(_.stripMargin.split(",")).map(_.toInt) match {
    case Array(a, b, c, d) => Line(((a, b), (c, d)))
    case _ => throw new Exception("What on earth")
  }

  def processLines[A](filepath: String)(f: String => A): Iterator[A] = {
    val source = Source.fromFile(filepath)
    val lines = source.getLines()
    lines.map(f)
  }

  def inputLines(filepath: String): Seq[Line] = processLines(filepath)(processLine).toSeq

  def allPoints(lines: Seq[Line]): Seq[Point] = lines.flatMap(_.points)

  def overlapping(lines: Seq[Line]) = allPoints(lines).groupBy(identity)

  def overlaps(lines: Seq[Line]) = overlapping(lines).count{case (_, b) => b.size > 1}

  def filterLines(lines: Seq[Line]): Seq[Line] = lines.filter(l => l.vertical | l.horizontal)

  def answer(p: Seq[Line] => Seq[Line]): String => Int = inputLines _ andThen p andThen overlaps

  def answerOne: String => Int = answer(filterLines)

  def answerTwo: String => Int = answer(identity)


  println(answerOne("src/fakeLines.txt"))
  println(answerTwo("src/lines.txt"))

//  inputLines("src/fakeLines.txt").map(l => (l, l.points)) foreach println

//  println(processLine("8,0 -> 0,8").points)
}
