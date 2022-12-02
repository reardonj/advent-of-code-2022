package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.SortedSet

object Day01 extends Day(1):
  override def a =
    readCalorieCounts
      .fold(0)(Math.max)
      .compile
      .lastOrError
      .flatMap(result => IO.println(s"Max calories: ${result}"))

  override def b =
    readCalorieCounts
      .fold(List.empty[Int]) { (top, next) => (next :: top).sortWith(_ > _).take(3) }
      .compile
      .lastOrError
      .flatMap(result => IO.println(s"Sum of top 3 calories: ${result.sum}"))

  private val readCalorieCounts = Files[IO]
    .readUtf8Lines(Path(s"data/day01/input.txt"))
    .groupAdjacentBy(_.nonEmpty)
    .collect { case (true, chunk) => chunk }
    .map(_.map(_.toInt).sumAll)
