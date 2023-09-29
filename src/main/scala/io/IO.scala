package io

import scala.util.Try

final class IO[A] {
  def map[B](f: A => B): IO[B] = ???
  def flatMap[B](f: A => IO[B]): IO[B] = ???
  def *>[B](another: IO[B]): IO[B] = ???
  def as[B](newValue: => B): IO[B] = ???
  def void: IO[Unit] = ???
  def attempt: IO[Either[Throwable, A]] = ???
  def option: IO[Option[A]] = ???
  def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] = ???
  def redeem[B](recover: Throwable => B, map: A => B): IO[B] = ???
  def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = ???
  def unsafeRunSync(): A = ???
}

object IO {
  def apply[A](body: => A): IO[A] = ???
  def suspend[A](thunk: => IO[A]): IO[A] = ???
  def delay[A](body: => A): IO[A] = ???
  def pure[A](a: A): IO[A] = ???
  def fromEither[A](e: Either[Throwable, A]): IO[A] = ???
  def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] = ???
  def fromTry[A](t: Try[A]): IO[A] = ???
  def none[A]: IO[Option[A]] = ???
  def raiseError[A](e: Throwable): IO[A] = ???
  def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = ???
  def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = ???
  def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = ???
  def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = ???
  val unit: IO[Unit] = ???
}
