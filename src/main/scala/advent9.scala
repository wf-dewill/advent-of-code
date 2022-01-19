import scala.util.{Success, Try}

object advent9 extends App {

  import processText.mapFromName

  val inputs = mapFromName("src/data/heightMap.txt")(_.toCharArray.map(_.toString.toInt)).toArray


  def findPoints(heightMap: Array[Array[Int]]) = {
    ((for {
      i <- heightMap.indices
      j <- heightMap.head.indices
    } yield (heightMap(i)(j), List(Try(heightMap(i-1)(j)), Try(heightMap(i)(j-1)), Try(heightMap(i+1)(j)), Try(heightMap(i)(j+1))), (i, j))) map {
      case (a, elem, coords) => (a, elem.collect{ case Success(b) => b }, coords)
    } ).toList
  }

  def findLows(points: List[(Int, List[Int], (Int, Int))]) = {
    // Use -1 to indicate non-low
    points.map {
      case (a, b, coords) => if (b.count(elem => a < elem) == b.length) (a, coords) else (-1, (-1,-1))
    }
  }

  def findRisk(lows: List[Int]) = {
    lows.map(_+1).sum
  }

  def answer1(ints: Array[Array[Int]]) = {
    (findPoints _ andThen findLows andThen {x => x.map(_._1)} andThen findRisk)(ints)
  }

  case class Point(i: Int, j: Int, value: Int) {
    val coords = (i, j)

    val coordsAround = Set((i+1, j), (i, j+1), (i-1, j), (i, j-1))

    def pointsAround(heightMap: Array[Array[Int]]) = {
      coordsAround.map { case (i, j) => (i, j, Try(heightMap(i)(j))) }
    }

    def validPoints(heightMap: Array[Array[Int]]) = {
      pointsAround(heightMap).collect {
        case (i, j, Success(value)) if value != 9 => Point(i, j, value)
      }
    }

  }

  def getPointsAround(point: Point, heightMap: Array[Array[Int]]) = {
    point.validPoints(heightMap)
  }

  def allPointsUntilEdge(point: Point, heightMap: Array[Array[Int]]) = {

    def inner(points: Set[Point], seen: Set[Point]): Set[Point] = {
      if (points.isEmpty) seen
      else inner(points.flatMap(p => getPointsAround(p, heightMap)).diff(seen), seen.union(points))}

    inner(getPointsAround(point, heightMap), Set())
  }

  def answer2(ints: Array[Array[Int]]) = {
    val points = (findPoints _ andThen findLows)(ints).filter{
      case (p, _) => p != -1
    }.map {
      case (v, (i, j)) => Point(i, j, v)
    }
    points.map(p => allPointsUntilEdge(p, ints).size).sorted.reverse.take(3).product
  }

//  println(answer1(inputs))
  val testInputs = mapFromName("src/data/smallHeightMap.txt")(_.toCharArray.map(_.toString.toInt)).toArray
  println(answer2( inputs))

}
