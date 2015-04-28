import cats._
import cats.data._
import cats.implicits._

def stringToInt(x: String): Validated[String, Int] = {
  try {
    Validated.valid(x.toInt)
  }
  catch {
    case _: NumberFormatException =>
      Validated.invalid(s"Invalid number $x")
  }
}
