object processText {

  import io.Source





  def getSource: String => Source = Source.fromFile _

  def lines: Source => Iterator[String] = _.getLines()

  def convert[T](f: String => T): Iterator[String] => Iterator[T] = _.map(f)

  def doAndClose(f: String => Unit): Source => Unit = {
    s =>
      s.getLines() foreach f
      s.close()
  }

  def mapFromName[T](filepath: String)(f: String => T) = (getSource andThen lines andThen convert(f))(filepath)

}
