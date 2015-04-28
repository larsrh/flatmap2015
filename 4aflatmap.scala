import cats._
import cats.implicits._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

def flatMapForOption[A, B, F[_] : Monad]
  (f: A => F[Option[B]], foa: F[Option[A]]): F[Option[B]] = {
    foa.flatMap {
      case None => Monad[F].pure(None: Option[B])
      case Some(a) => f(a)
    }
}

object funcs {

  def factorsPresent(n: Int): List[Option[Int]] =
    (1 to n).map { i =>
      if (n % i == 0)
        Some(n / i)
      else
        None
    }.toList

  def expensiveComputation(n: Int): Future[Option[Int]] =
    if (n == 1)
      Future { None }
    else if (n % 2 == 0)
      Future { Some(n / 2) }
    else
      Future { Some(n * 3 + 1) }



  def flatMapForEither[A, B, E, F[_] : Monad]
    (f: A => F[Either[E, B]], feea: F[Either[E, A]]): F[Either[E, B]] = ???

}
