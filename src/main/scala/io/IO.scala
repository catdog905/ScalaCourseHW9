package io

import scala.util.{Failure, Success, Try}

final class IO[A](val impureCompute: () => A) {
  def map[B](f: A => B): IO[B] = IO(f(unsafeRunSync()))
  def flatMap[B](f: A => IO[B]): IO[B] = IO(f(unsafeRunSync()).unsafeRunSync())
  def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
  def as[B](newValue: => B): IO[B] = map(_ => newValue)
  def void: IO[Unit] = map(_ => ())
  def attempt: IO[Either[Throwable, A]] = IO(Try(unsafeRunSync()).toEither)
  def option: IO[Option[A]] = attempt.map(_.toOption)
  def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
    redeemWith(f, IO.pure)
  def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
    attempt.map({
      case Left(throwable) => recover(throwable)
      case Right(value)    => map(value)
    })

  def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] =
    attempt.flatMap({
      case Left(throwable) => recover(throwable)
      case Right(value)    => bind(value)
    })
  def unsafeRunSync(): A = impureCompute()
}

object IO {
  def apply[A](body: => A): IO[A] = IO.delay(body)
  def suspend[A](thunk: => IO[A]): IO[A] = IO(thunk.unsafeRunSync())
  def delay[A](body: => A): IO[A] = new IO(() => body)
  def pure[A](a: A): IO[A] = IO(a)
  def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
    case Left(exception) => IO.raiseError(exception)
    case Right(value)    => IO.pure(value)
  }
  def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = option match {
    case Some(value) => IO.pure(value)
    case None        => raiseError(orElse)
  }
  def fromTry[A](t: Try[A]): IO[A] = t match {
    case Success(value)     => IO.pure(value)
    case Failure(exception) => IO.raiseError(exception)
  }
  def none[A]: IO[Option[A]] = pure(None)
  def raiseError[A](e: Throwable): IO[A] = IO.delay(throw e)
  def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = whenA(!cond)(raiseError(e))
  def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = whenA(cond)(raiseError(e))
  def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = whenA(!cond)(action)
  def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) {
    action
  } else {
    IO.unit
  }
  val unit: IO[Unit] = pure(())
}
