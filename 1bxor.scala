import cats._
import cats.data._

def stringToInt(x: String): Xor[String, Int] = {
  try {
    Xor.right(x.toInt)
  }
  catch {
    case _: NumberFormatException =>
      Xor.left(s"Invalid number $x")
  }
}
