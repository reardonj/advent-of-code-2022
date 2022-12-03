package com.jmreardon.aoc2022

import cats.effect.IO

trait Day(date: Int):
  def a = fs2.Stream[IO, String]("Not implemented")
  def b = fs2.Stream[IO, String]("Not implemented")

  def entries = Map(s"${date}a" -> a, s"${date}b" -> b)
