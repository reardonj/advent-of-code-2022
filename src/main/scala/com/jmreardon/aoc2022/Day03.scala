package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._
import scala.collection.mutable.PriorityQueue
import scala.collection.immutable.SortedSet

object Day03 extends Day(3):
  override def a =
    readFile
      .map(line =>
        line.splitAt(line.length / 2) match {
          case (a, b) =>
            extractContents(a).intersect(extractContents(b)).toList.map(priority).sum
        }
      )
      .fold(0)(_ + _)
      .map(_.toString)

  override def b =
    readFile
      .map(extractContents)
      .chunkN(3, false)
      .map(_.reduceLeftOption(_ intersect _).getOrElse(Set.empty).map(priority).sum)
      .fold(0)(_ + _)
      .map(_.toString)

  private val readFile = Files[IO].readUtf8Lines(Path(s"data/day03/input.txt")).filter(_.nonEmpty)

  private def extractContents(bagContents: String) = bagContents.toSet

  private def priority(c: Char): Int = 1 + c - (if c >= 'a' then 'a' else ('A' - 26))
