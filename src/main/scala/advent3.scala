import sun.tools.java.Package

import scala.annotation.tailrec

import scala.io.Source

object advent3 extends App {
  import scala.collection.{Map, mutable}
  import scala.collection.immutable.{HashMap, Map}
  val trial = List(
    "00100",
  "11110",
  "10110",
  "10111",
  "10101",
  "01111",
  "00111",
  "11100",
  "10000",
  "11001",
  "00010",
  "01010",
  )

  def gammaRate(bits: Seq[String]): String = bits.transpose.map(xs => xs.maxBy(elem => xs.count(_ == elem))).mkString("")
  def epsilonConvert(gamma: String): String = gamma.map {
    case '1' => '0'
    case '0' => '1'
  }.mkString("")
  def epsilonRate(bits: Seq[String]): String = epsilonConvert(gammaRate(bits))

  def convertBinary(binary: String): Int = {
    binary.reverse.zipWithIndex.map { case (a, b) => a.asDigit*Math.pow(2, b)}.sum.toInt
  }

  def powerConsumption(bits: Seq[String]): Int = {
    val gamma = gammaRate(bits)
    val epsilon = epsilonConvert(gamma)
    List(gamma, epsilon).map(convertBinary).product
  }

  assert(gammaRate(trial) == "10110")
  assert(epsilonConvert(gammaRate(trial)) == "01001")
//  println(powerConsumption(trial))

  val source = Source.fromFile("src/binaries.txt")
//  println(powerConsumption(source.getLines().toSeq))



    @tailrec
    def recurFilter(pos: Int, bits: Seq[String], equalsValue: Char, comb: (Int, Int) => Boolean): String = {
      if (bits.size == 1) bits.head
      else recurFilter(pos + 1, bits.filter(elem => elem(pos) == commonAtPosition(bits, pos, equalsValue, comb)), equalsValue, comb)
    }


  /*
  * Not the most efficient, takes a while to run ( a while be 3 seconds, but still not as fast as I'd like)
  * */
  def commonAtPosition(bits: Seq[String], pos: Int, equalsValue: Char, comb: (Int, Int) => Boolean)= {
    bits
      .transpose
      .toList(pos)
      .groupBy(identity).toList.map { case (a, b) => (a, b.size) } match {
      case List((c, a), (d, b)) => if (a == b) equalsValue else if (comb(a, b)) c else d
    }
  }



  def oxygen(bits: Seq[String]): String = recurFilter(0, bits, '1', (a: Int, b: Int) => a>b)
  def carbon(bits: Seq[String]): String = recurFilter(0, bits, '0', (a: Int, b: Int) => a<b)

  def oxyCarb(bits: Seq[String]): Int = convertBinary(oxygen(bits)) * convertBinary(carbon(bits))
  assert(oxyCarb(trial) == 230)

  print(oxyCarb(source.getLines().toSeq))
}
