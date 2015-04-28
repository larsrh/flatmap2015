import cats._
import cats.implicits._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

case class OptionT[M[_], A](run: M[Option[A]]) {

  def flatMap[B](f: A => OptionT[M, B])(implicit M: Monad[M]): OptionT[M, B] =
    OptionT(run.flatMap {
      case None =>
        M.pure(None: Option[B])
      case Some(a) =>
        f(a).run
    })

  def map[B](f: A => B)(implicit M: Functor[M]): OptionT[M, B] =
    OptionT(run.map(_.map(f)))

}

object funcs {

  def factorsPresentT(n: Int): OptionT[List, Int] =
    OptionT(
      (1 to n).map { i =>
        if (n % i == 0)
          Some(n / i)
        else
          None
      }.toList
    )

  def expensiveComputationT(n: Int): OptionT[Future, Int] =
    OptionT(
      if (n == 1)
        Future { None }
      else if (n % 2 == 0)
        Future { Some(n / 2) }
      else
        Future { Some(n * 3 + 1) }
    )

}
