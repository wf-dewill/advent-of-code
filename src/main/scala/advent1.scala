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

  println(trial.sliding(2).count {
    case List(a, b) => b > a
    case _ => false
  })

}
