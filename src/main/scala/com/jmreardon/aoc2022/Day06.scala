package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._
import cats.Functor
import cats.Foldable

object Day06 extends Day(6):
  override def a =
    readFile.through(findDistinctMarker(4)).fold("")((a, b) => a + " " + b)

  override def b =
    readFile.through(findDistinctMarker(14)).fold("")((a, b) => a + " " + b)

  private def findDistinctMarker(count: Int)(line: String) =
    fs2
      .Stream
      .iterable(line)
      .sliding(count)
      .takeWhile(_.toList.toSet.size != count)
      .through(countResults)
      .map(total => total.toInt + count)

  private val readFile = Files[IO].readUtf8Lines(Path(s"data/day06/input.txt"))
