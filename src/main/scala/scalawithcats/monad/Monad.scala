package scalawithcats.monad

import cats.Monad

import scala.concurrent.Future

trait Monad[F[_]] {
  def pure[A](a: A): F[A]

  def flatMap[A,B](value: F[A])(func: A => F[B]): F[B]

  def map[A, B](value: F[A])(func: A => B): F[B] =
    flatMap(value)(a => pure(func(a)))
}

object Dtuff {
  import scala.concurrent.ExecutionContext.Implicits.global
  val fm = Monad[Future]
}