import scala.io.{BufferedSource, Source}

object advent4 extends App {
  case class Board(nums: Array[Array[Int]]) {
    type Line = Array[(Int, Boolean)]

    override def toString: String = s"Array(${internal.map(_.mkString(", ")).mkString("Array(", ", ", ")")})"

    var internal: Array[Line] = nums.map(_.map(elem => (elem, false)))
    lazy val justNums: Array[Int] = nums.flatten

    def row(n: Int): Line = internal(n)
    def col(n: Int): Line = internal.map(_(n))

    def isMarked(line: Line): Boolean = !line.exists{case (_, b) => !b}

    def check(n: Int)(f: Int => Line): Boolean = (f andThen isMarked)(n)
    def checkRow(n: Int): Boolean = check(n)(row)
    def checkCol(n: Int): Boolean = check(n)(col)

    def inBoard(n: Int): Boolean = justNums.contains(n)

    def updateElement(rowy: Int, coly: Int): Array[Line] = {
      internal.updated(rowy, internal(rowy).updated(coly, (internal(rowy)(coly)._1, true)))
    }

    def updateAndCheck(n: Int): Boolean =
      if (!inBoard(n)) false
      else {
        val k = justNums.indexOf(n)
        val (i, j) = (k/5.floor.toInt, k%5)
        internal = updateElement(i, j)
        checkRow(i) | checkCol(j)
      }

    def unmarkedNumbers: Array[Int] = internal.flatMap(_.filter { case (_,b) => !b}.map(_._1))

  }


  def processString(string: String): Array[Int] = string.stripMargin.replace("  ", " ").split(" ").filter(_.nonEmpty).map(_.toInt)

  def checkString(string: String): Array[Int] = if (string.isEmpty) Array() else {
    processString(string)
  }

  def convert(sourcy: Source): Iterator[Array[Int]] = {
    sourcy.getLines().map(checkString)
  }

  def boardsFromIter(iter: Iterator[Array[Int]]): List[Board] = {
    iter.sliding(5,6).map(a => Board(a.toArray)).toList
  }

  def boards(source: Source): List[Board] = (boardsFromIter _ compose convert)(source)

  val source = Source.fromFile("src/boards.txt")

  val trial = Board(
    Array(
      Array(1,2,3,4,5),
      Array(6,7,8,9,10),
      Array(11,12,13,14,15),
      Array(16,17,18,19,20),
      Array(21,22,23,24,25)
    )
  )
//  for {i <- source.getLines()} println(checkString(i).map(_.mkString("")).sliding(5, 5).toList)

//  trial.nums.sliding(5, 6).foreach(a => println(a.map(_.mkString("Array(", ",", ")")).mkString("Array(", ",", ")")))

  def processInputs(source: Source) = source.getLines().flatMap(_.split(",")).map(_.toInt)

  def winningBoards(pathToInputs: String, pathToBoards: String) = {
    var boardsNow = boards(Source.fromFile(pathToBoards))
    val inputsNow = processInputs(Source.fromFile(pathToInputs))
    for {
      n <- inputsNow
      board <- boardsNow
      if board.updateAndCheck(n)
    } yield {
      boardsNow = boardsNow.filter(_ != board)
      (n, board)
    }
  }

  def firstAnswer(pathToInputs: String = "src/boardInputs.txt", pathToBoards: String = "src/boards.txt") = {
    val (num, board) = winningBoards(pathToInputs, pathToBoards).next()
    board.unmarkedNumbers.sum * num
  }

  def secondAnswer(pathToInputs: String = "src/boardInputs.txt", pathToBoards: String = "src/boards.txt") = {
    val (num, board) = winningBoards(pathToInputs, pathToBoards).toList.last
    board.unmarkedNumbers.sum * num
  }

  println(secondAnswer())
}
