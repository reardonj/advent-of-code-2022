package com.jmreardon.aoc2022

import cats.effect.IO

trait Day(date: Int):
  def a = fs2.Stream[IO, String]("Not implemented")
  def b = fs2.Stream[IO, String]("Not implemented")

  def entries = Map(s"${date}a" -> a, s"${date}b" -> b)

  protected final def countResults[A](stream: fs2.Stream[IO, A]) =
    fs2.Stream.eval(stream.compile.count).map(_.toString)
