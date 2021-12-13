import scala.io.Source

object advent2 extends App{

  trait Movement
  trait Horizontal extends Movement
  case class Forward(x: Int) extends Horizontal

  trait Vertical extends Movement
  case class Down(x: Int) extends Vertical
  case class Up(x: Int) extends Vertical

  case class Posititon(depth: Int, hoz: Int) extends Posititonable {
    def edit(move: Movement): Posititon = move match {
      case Up(x) => Posititon(depth - x, hoz)
      case Down(x) => Posititon(depth + x, hoz)
      case Forward(x) => Posititon(depth, hoz + x)
    }
  }

  trait Posititonable {
    def edit(move: Movement): Posititonable
  }

  def changePosition(position: Posititonable, movements: Iterator[Movement]) = movements.foldLeft(position){case (pos, move) => {pos.edit(move)}}

  def parser(name: String, amount: String): Movement = {
    if (name == "forward") Forward(amount.toInt)
    else if (name == "down") Down(amount.toInt)
    else if (name == "up") Up(amount.toInt)
    else throw new Exception("Unknown movement")
  }
  def splitLine(line: String): (String, String) = line.split(" ") match {
    case Array(x, y) => (x, y)
  }

  def processLine(string: String): Movement = (splitLine _ andThen { case (a, b) => parser(a, b)})(string)

  def answer(filepath: String, position: Posititonable) = {
    val source = Source.fromFile(filepath)
    val movements: Iterator[Movement] = source.getLines().map(processLine)

    changePosition(position, movements = movements) match {
      case Posititon(a, b) => a*b
      case AimablePosition(a, b, _) => a*b
    }
  }

  def test(position: Posititonable) = {
    val trial = List("forward 5",
    "down 5",
    "forward 8",
    "up 3",
    "down 8",
    "forward 2",
    ).toIterator.map(processLine)

    changePosition(position, movements = trial) match {
      case Posititon(a, b) => a*b
      case AimablePosition(a, b, _) => a*b
    }
  }



  case class AimablePosition(depth: Int, hoz: Int, aim: Int) extends Posititonable {
    def edit(move: Movement): AimablePosition = move match {
      case Up(x) => AimablePosition(depth , hoz, aim - x)
      case Down(x) => AimablePosition(depth, hoz, aim + x)
      case Forward(x) => AimablePosition(depth + (aim*x), hoz + x, aim)
    }
  }

  //  println(test(AimablePosition(0, 0, 0)))
    println(answer("src/positions.txt", AimablePosition(0,0,0)))

}
