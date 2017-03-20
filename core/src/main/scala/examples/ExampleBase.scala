package examples

/**
  * @author paulius
  */
trait ExampleBase {
  def attempt[A](f: => A): Either[A, String] = {
    try Left(f) catch {
      case e: NoSuchMethodException => Right(e.getClass.toString)
    }
  }
}
