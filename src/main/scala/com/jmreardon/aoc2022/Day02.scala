package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._

object Day02 extends Day(2):
  override def a = readInput.map(readMatch).map(scoreMatch).fold(0)(_ + _).map(_.toString)

  override def b = readInput
    .map(readMatchFromOutcome)
    .map(scoreMatch)
    .fold(0)(_ + _)
    .map(_.toString)

  private val readInput = Files[IO].readUtf8Lines(Path(s"data/day02/input.txt")).filter(_.nonEmpty)

  private def readMatch(line: String) =
    (readChoice('A', 'B', 'C')(line(0)), readChoice('X', 'Y', 'Z')(line(2)))

  private def readMatchFromOutcome(line: String) =
    val opponent = readChoice('A', 'B', 'C')(line(0))
    val outcome = line(2) match {
      case 'X' => Outcome.Loss
      case 'Y' => Outcome.Draw
      case 'Z' => Outcome.Win
      case _   => throw IllegalArgumentException(s"Unexpected input '$line'")
    }

    (opponent, choiceToGetOutcome(opponent, outcome))

  private def scoreMatch(theMatch: (Choice, Choice)) =
    theMatch._2.outcome(theMatch._1).value + theMatch._2.value

  private def readChoice(rock: Char, paper: Char, scissors: Char)(input: Char) = input match {
    case `rock`     => Choice.Rock
    case `paper`    => Choice.Paper
    case `scissors` => Choice.Scissors
    case _          => throw IllegalArgumentException(s"Unexpected input '$input'")
  }

  private val choiceToGetOutcome = (
    for {
      oppChoice <- Choice.values
      myChoice <- Choice.values
    } yield (oppChoice, myChoice.outcome(oppChoice)) -> myChoice
  ).toMap

  private enum Outcome(val value: Int):
    case Win extends Outcome(6)
    case Loss extends Outcome(0)
    case Draw extends Outcome(3)

  private enum Choice(val value: Int):
    def outcome(opponent: Choice): Outcome = (this, opponent) match {
      case (Rock, Scissors)  => Outcome.Win
      case (Paper, Rock)     => Outcome.Win
      case (Scissors, Paper) => Outcome.Win
      case (a, b) if a == b  => Outcome.Draw
      case _                 => Outcome.Loss
    }

    case Rock extends Choice(1)
    case Paper extends Choice(2)
    case Scissors extends Choice(3)
