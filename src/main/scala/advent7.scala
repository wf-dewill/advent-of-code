object advent7 extends App {

  val elems = List(16,1,2,0,4,2,7,1,2,14)


  def median(nums: Seq[Int]): Int = {
    val theNums = nums.sorted
    if (nums.size % 2==0) Seq(theNums((nums.size/2) - 1 ), theNums(nums.size/2)).sum/2
    else theNums(nums.size / 2)
  }

  def mean(nums: Seq[Int]): Int = {
    (nums.sum/nums.size)
  }

  def meanMedian(nums: Seq[Int]): Int = {
    Seq(mean(nums), median(nums)).sum/2
  }

  def fuel(nums: Seq[Int])(combiner: Seq[Int] => Int): Int = {
    val optimal = combiner(nums)
    nums.map(a => math.abs(a-optimal)).sum
  }

  def fuel2(nums: Seq[Int])(combiner: Seq[Int] => Int): Int = {
    val optimal = combiner(nums)
    println(optimal)
    nums.map(a =>
      if (a > optimal) (optimal to a).map(_ - optimal).sum
      else if (a < optimal) (a to optimal).map(_ - a).sum
      else 0
    ).sum
  }

  import processText.mapFromName

  val inputs = mapFromName("src/data/crab.txt")(a => a.split(",")).flatten.map(_.toInt).toSeq

  def answerGeneral(filepath: String)(fuelFunc: Seq[Int] => (Seq[Int] => Int) => Int)(combiner: Seq[Int] => Int): Int = {
    fuelFunc(mapFromName(filepath)(a => a.split(",")).flatten.map(_.toInt).toSeq)(combiner)
  }

//  def answer1(filepath: String): Int = answerGeneral(filepath)(fuel)(median)
//
//  def answer2(filepath: String): Int = answerGeneral(filepath)(fuel2)(mean)


//  println(answerGeneral("src/data/crab.txt")(fuel2)(meanMedian))
//  println(answerGeneral("src/data/crab.txt")(fuel2)(median))
  println(answerGeneral("src/data/crab.txt")(fuel2)(mean))


  def fuel3(nums: Seq[Int])(optimal: Int): Int = {
    nums.map(a =>
      if (a > optimal) (optimal to a).map(_ - optimal).sum
      else if (a < optimal) (a to optimal).map(_ - a).sum
      else 0
    ).sum
  }

  def minimise[S](func: Seq[Int] => (Int) => Int)(seq: Seq[Int])(a: Int, b: Int): Int = {
    println(a, b)
    if (a == b) a
    else {
      val (aAns, bAns) = (func(seq)(a), func(seq)(b))
      if (aAns > bAns) minimise(func)(seq)((a+b)/2, b)
      else minimise(func)(seq)(a, (a+b)/2)
    }

  }


//  println(minimise(fuel3)(inputs)(inputs.min, inputs.max))

  println(fuel3(inputs)(473))

}
