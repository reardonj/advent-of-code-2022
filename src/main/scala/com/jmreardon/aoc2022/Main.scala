package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.ExitCode
import java.time.ZoneId

object Main extends IOApp {

  private val days: Map[String, IO[Unit]] =
    Day01.entries

  // This is your new "main"!
  def run(args: List[String]): IO[ExitCode] =
    val challenges =
      if args.isEmpty then
        IO.realTimeInstant
          .map { instant =>
            val date = instant.atZone(ZoneId.systemDefault).getDayOfMonth

            List("a", "b").map(part => s"$date$part")
          }
      else IO.pure(args)

    challenges.flatMap(_.map(runChallenge).sequence_) >> IO.pure(ExitCode.Success)

  private def runChallenge(challengeId: String) =
    for {
      _ <- IO.println(s"Challenge $challengeId:")
      start <- IO.monotonic
      _ <- days.getOrElse(challengeId, IO.println("Challenge does not exist"))
      end <- IO.monotonic

      runTime = end - start
      _ <- IO.println(s"(run time: ${runTime.toMillis}ms)")
      _ <- IO.println("")
    } yield IO.unit

}