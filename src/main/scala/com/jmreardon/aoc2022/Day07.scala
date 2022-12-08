package com.jmreardon.aoc2022

import cats._
import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._
import cats.Functor
import cats.Foldable

object Day07 extends Day(7):
  override def a = readFile.map(unfold(_).filter(_.size < 100000).map(_.size).combineAll.toString)

  override def b = readFile.map { dirs =>
    val dirList = unfold(dirs)
    val usedSpace = 70000000 - dirList.head.size
    val spaceNeeded = 30000000 - usedSpace

    dirList.filter(_.size > spaceNeeded).minBy(_.size).size.toString
  }

  private val readFile = Files[IO]
    .readUtf8Lines(Path(s"data/day07/input.txt"))
    .filter(_.nonEmpty)
    .fold(List.empty[Dir]) {
      case (dirs, s"$$ ls")                   => dirs
      case (left :: above :: dirs, "$ cd ..") => above.add(left) :: dirs
      case (dirs, s"$$ cd $name")             => Dir(name, List.empty) :: dirs
      case (dirs, s"dir $_")                  => dirs
      case (curr :: dirs, s"$size $name")     => curr.add(File(name, size.toLong)) :: dirs
      case _                                  => ???
    }
    .map(_.reduce((curr, above) => above add (curr)))

  private trait ElfFS:
    def size: Long

  private case class Dir(dir: String, content: List[ElfFS]) extends ElfFS {
    def add(newContent: ElfFS) = this.copy(content = newContent :: content)

    lazy val size = content.foldMap(_.size)
  }

  private case class File(name: String, size: Long) extends ElfFS

  private def unfold(dir: Dir): List[Dir] =
    dir :: dir.content.collect { case d: Dir => unfold(d) }.flatten
