def stringToInt(x: String): Either[String, Int] = {
  try {
    Right(x.toInt)
  }
  catch {
    case _: NumberFormatException =>
      Left(s"Invalid number $x")
  }
}
