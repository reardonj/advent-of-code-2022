package com.jmreardon.aoc2022

import cats.effect.IO

trait Day(date: Int):
  def a = IO.println("Not implemented")
  def b = IO.println("Not implemented")

  def entries = Map(s"${date}a" -> a, s"${date}b" -> b)
