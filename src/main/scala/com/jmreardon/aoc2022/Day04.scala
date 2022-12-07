package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._

object Day04 extends Day(4):
  override def a =
    readFile.filter((a, b) => a.contains(b) || b.contains(a)).through(countResults)

  override def b =
    readFile.filter((a, b) => a.overlaps(b)).through(countResults)

  private final case class Assignment(start: Int, end: Int):
    def contains(other: Assignment) = start <= other.start && end >= other.end
    def contains(section: Int) = section >= start && section <= end
    def overlaps(other: Assignment) =
      contains(other) || other.contains(start) || other.contains(end)

  private val readFile = Files[IO]
    .readUtf8Lines(Path(s"data/day04/input.txt"))
    .filter(_.nonEmpty)
    .map(parseAssignmentPair)

  private def parseAssignmentPair(line: String) =
    line.split("[,-]").map(_.toInt) match {
      case Array(a1, a2, b1, b2) => (Assignment(a1, a2), Assignment(b1, b2))
      case _                     => ???
    }
