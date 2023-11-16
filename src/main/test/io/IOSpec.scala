package io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.util.{Failure, Success}

class IOSpec extends AnyFlatSpec with Matchers {

  "map" should "correctly apply the function" in {
    val io = IO(5)
    val mapped = io.map(_ * 2)
    mapped.unsafeRunSync() should be(10)
  }

  "flatMap" should "correctly apply the function" in {
    val io = IO(3)
    val flatMapped = io.flatMap(x => IO(x.toString))
    flatMapped.unsafeRunSync() should be("3")
  }

  "*> operator" should "correctly discard the first result" in {
    val io1 = IO(println("Side effect 1"))
    val io2 = IO(42)
    (io1 *> io2).unsafeRunSync() should be(42)
  }

  "*> operator" should "correctly executes the first IO" in {
    var size_effect = 42
    val io1 = IO { size_effect = size_effect + 1 }
    val io2 = IO(42)
    (io1 *> io2).unsafeRunSync()
    size_effect should be(43)
  }

  "as" should "correctly replace the value" in {
    val io = IO(5)
    val replaced = io.as("replaced")
    replaced.unsafeRunSync() should be("replaced")
  }

  "void" should "produce unit" in {
    val io = IO(5)
    io.void.unsafeRunSync() should be(())
  }

  "attempt" should "correctly handle success and failure" in {
    val successIO = IO(5)
    val failureIO = IO(throw new RuntimeException("Error"))

    successIO.attempt.unsafeRunSync() should be(Right(5))
    failureIO.attempt.unsafeRunSync().isLeft should be(true)
  }

  "option" should "correctly convert to Option" in {
    val successIO = IO(5)
    val failureIO = IO(throw new RuntimeException("Error"))

    successIO.option.unsafeRunSync() should be(Some(5))
    failureIO.option.unsafeRunSync() should be(None)
  }

  "handleErrorWith" should "correctly handle errors" in {
    val failureIO = IO(throw new RuntimeException("Error"))
    val recoveredIO = failureIO.handleErrorWith(_ => IO("Recovered"))

    recoveredIO.unsafeRunSync() should be("Recovered")
  }

  "redeem" should "correctly apply functions" in {
    val successIO = IO(5)
    val failureIO = IO(throw new RuntimeException("Error"))

    successIO.redeem(_ => "Failed", _.toString).unsafeRunSync() should be("5")
    failureIO.redeem(_ => "Recovered", _.toString).unsafeRunSync() should be("Recovered")
  }

  "redeemWith" should "correctly handle success and failure" in {
    val successIO = IO(5)
    val failureIO = IO(throw new RuntimeException("Error"))

    successIO.redeemWith(_ => IO("Recovered"), x => IO(x.toString)).unsafeRunSync() should be("5")
    failureIO.redeemWith(_ => IO("Recovered"), x => IO(x.toString)).unsafeRunSync() should be("Recovered")
  }

  "unsafeRunSync" should "correctly execute IO" in {
    val io = IO(5)
    io.unsafeRunSync() should be(5)
  }

  "apply" should "create a new IO instance" in {
    val io = IO(42)
    io.unsafeRunSync() should be(42)
  }

  "suspend" should "defer the execution of an IO instance" in {
    var sideEffect = 0
    val io = IO {
      sideEffect += 1; sideEffect
    }
    val suspended = IO.suspend(io)
    sideEffect should be(0)
    suspended.unsafeRunSync() should be(1)
  }

  "delay" should "defer the computation until run" in {
    var sideEffect = 0
    val delayed = IO.delay {
      sideEffect += 1; sideEffect
    }
    sideEffect should be(0)
    delayed.unsafeRunSync() should be(1)
  }

  "pure" should "create an IO instance with a precomputed value" in {
    val io = IO.pure(99)
    io.unsafeRunSync() should be(99)
  }

  "fromEither" should "create an IO instance from an Either" in {
    val rightIO = IO.fromEither(Right(100))
    val leftIO = IO.fromEither(Left(new RuntimeException("Error")))

    rightIO.unsafeRunSync() should be(100)
    an[RuntimeException] should be thrownBy leftIO.unsafeRunSync()
  }

  "fromOption" should "create an IO instance from an Option" in {
    val someIO = IO.fromOption(Some(123))(new RuntimeException("Empty"))
    val noneIO = IO.fromOption(None)(new RuntimeException("Empty"))

    someIO.unsafeRunSync() should be(123)
    an[RuntimeException] should be thrownBy noneIO.unsafeRunSync()
  }

  "fromTry" should "create an IO instance from a Try" in {
    val successIO = IO.fromTry(Success(456))
    val failureIO = IO.fromTry(Failure(new RuntimeException("Error")))

    successIO.unsafeRunSync() should be(456)
    an[RuntimeException] should be thrownBy failureIO.unsafeRunSync()
  }

  "none" should "create an IO instance representing None" in {
    val noneIO = IO.none[Int]
    noneIO.unsafeRunSync() should be(None)
  }

  "raiseError" should "create an IO instance that throws an error" in {
    val errorIO = IO.raiseError(new RuntimeException("Error"))
    an[RuntimeException] should be thrownBy errorIO.unsafeRunSync()
  }

  "raiseUnless" should "raise error if condition is false" in {
    val trueConditionIO = IO.raiseUnless(cond = true)(new RuntimeException("Error"))
    val falseConditionIO = IO.raiseUnless(cond = false)(new RuntimeException("Error"))

    trueConditionIO.unsafeRunSync() should be(())
    an[RuntimeException] should be thrownBy falseConditionIO.unsafeRunSync()
  }

  "raiseWhen" should "raise error if condition is true" in {
    val trueConditionIO = IO.raiseWhen(cond = true)(new RuntimeException("Error"))
    val falseConditionIO = IO.raiseWhen(cond = false)(new RuntimeException("Error"))

    an[RuntimeException] should be thrownBy trueConditionIO.unsafeRunSync()
    falseConditionIO.unsafeRunSync() should be(())
  }

  "unlessA" should "execute action if condition is false" in {
    var sideEffect = 0
    val action = IO {
      sideEffect += 1
    }
    IO.unlessA(cond = false)(action).unsafeRunSync()

    sideEffect should be(1)
  }

  "whenA" should "execute action if condition is true" in {
    var sideEffect = 0
    val action = IO {
      sideEffect += 1
    }
    IO.whenA(cond = true)(action).unsafeRunSync()

    sideEffect should be(1)
  }

  "unit" should "represent a precomputed unit" in {
    IO.unit.unsafeRunSync() should be(())
  }

  // Stack-safeness tests
//  "flatMap" should "be stack safe" in {
//    val largeNumber = 10000
//
//    @tailrec
//    def loop(n: Int, acc: IO[Int]): IO[Int] =
//      if (n <= 0) acc
//      else loop(n - 1, acc.flatMap(x => IO(x + 1)))
//
//    noException should be thrownBy loop(largeNumber, IO(0)).unsafeRunSync()
//    loop(largeNumber, IO(0)).unsafeRunSync() should be(largeNumber)
//  }
}
