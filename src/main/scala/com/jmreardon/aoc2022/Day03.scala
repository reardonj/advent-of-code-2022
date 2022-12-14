package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._
import cats.Functor
import cats.Foldable

object Day03 extends Day(3):
  override def a =
    readFile.foldMap(line => contentPriority(line.splitAt(line.length / 2).toList)).map(_.toString)

  override def b = readFile.chunkN(3, false).foldMap(contentPriority).map(_.toString)

  private val readFile = Files[IO].readUtf8Lines(Path(s"data/day03/input.txt")).filter(_.nonEmpty)

  private def contentPriority[F[_]: Functor: Foldable](contents: F[String]) =
    contents.map(_.toSet).reduceLeftOption(_ intersect _).getOrElse(Set.empty).map(priority).sum

  private def priority(c: Char): Int = 1 + c - (if c >= 'a' then 'a' else ('A' - 26))
