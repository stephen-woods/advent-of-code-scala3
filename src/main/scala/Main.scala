import zio.*
import zio.prelude.*
import scala.util.Try

object Main extends ZIOAppDefault {

  override def run = {
    val effect = for {
      args <- getArgs
      _    <- validateArgs(args, runDay)
    } yield ()

    effect.catchAll { th =>
      Console.printError(th.getMessage())
    }
  }

  def validateArgs(
      args: Chunk[String],
      vf: (Int, Int) => Task[Unit]
  ): Task[Unit] = {
    args match {
      case Chunk(year, day) =>
        val vYear = Try(year.toInt).fold(
          fa = e => Validation.fail(s"Year must be a number: $year"),
          fb = year => Validation.succeed(year)
        )

        val vDay = Try(day.toInt).fold(
          fa = e => Validation.fail(s"Year must be a number: $year"),
          fb = day => Validation.succeed(day)
        )

        Validation
          .validateWith(vYear, vDay)(Tuple2.apply)
          .mapError(msg => new RuntimeException(msg))
          .toZIO
          .flatMap { case (year, day) =>
            vf(year, day)
          }

      case _ =>
        ZIO.fail(new java.lang.RuntimeException("Usage run <year> <day>"))
    }
  }

  def runDay(year: Int, day: Int): Task[Unit] = {
    for {
      _ <- Console.printLine(s"Advent of code $year in Scala 3!")
      _ <- (year, day) match {
             case (2021, 1) => year2021.Day01.run()
             case (2021, 2) => year2021.Day02.run()
             case (2023, 1) => year2023.Day01.run()
             case (2023, 2) => year2023.Day02.run()
             case _         =>
               Console.printLineError(s"--- Day $day $year: Not Implemented!")
           }
    } yield ()
  }
}
