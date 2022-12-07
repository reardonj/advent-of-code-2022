package com.jmreardon.aoc2022

import cats.syntax.all._
import cats.effect.syntax.all._
import cats.effect._
import fs2.io.file._
import cats.data.Chain

object Day05 extends Day(5):
  override def a = readFile(StackingState._9000)

  override def b = readFile(StackingState._9001)

  private final case class Assignment(start: Int, end: Int):
    def contains(other: Assignment) = start <= other.start && end >= other.end
    def contains(section: Int) = section >= start && section <= end
    def overlaps(other: Assignment) =
      contains(other) || other.contains(start) || other.contains(end)

  private def readFile(stacker: StackingState) = Files[IO]
    .readUtf8Lines(Path(s"data/day05/input.txt"))
    .fold(stacker)(_ readLine _)
    .evalTap(_.stacks.toList.sortBy(_._1).traverse(IO.println))
    .map(_.stacks.toList.sortBy(_._1).map(_._2.head).mkString)

  private type Stacks = Map[Int, List[Char]]

  private case class StackingState(
      step: StackingStep,
      stacks: Stacks,
      move: (String, Stacks) => Stacks
  ):
    def readLine(line: String): StackingState = step match
      case StackingStep.ReadInitialStacks =>
        if (line.isEmpty) then
          this.copy(step = StackingStep.MovingCargo, stacks = stacks.mapValues(_.reverse).toMap)
        else this.copy(stacks = readStacks(line, 1, stacks))
      case StackingStep.MovingCargo => this.copy(stacks = move(line, stacks))

  private enum StackingStep:
    case ReadInitialStacks
    case MovingCargo

  private object StackingState:
    val _9000 = StackingState(StackingStep.ReadInitialStacks, Map(), moveCargo)
    val _9001 = StackingState(StackingStep.ReadInitialStacks, Map(), moveCargoBulk)

    private def moveCargo(line: String, stacks: Stacks): Stacks = line match {
      case s"move $boxes from $initialStack to $finalStack" =>
        (1 to boxes.toInt).foldLeft(stacks) { (s, _) =>
          val toMove = s.get(initialStack.toInt).flatMap(_.headOption)
          s.updatedWith(initialStack.toInt)(_.map(_.tail))
            .updatedWith(finalStack.toInt)(toMove.zip(_).map(_ :: _))
        }

      case "" => stacks
      case _  => throw IllegalStateException(s"Unexpected input: $line")
    }

    private def moveCargoBulk(line: String, stacks: Stacks): Stacks = line match {
      case s"move $boxes from $initialStack to $finalStack" =>
        val toMove = stacks.get(initialStack.toInt).map(_.take(boxes.toInt))
        stacks
          .updatedWith(initialStack.toInt)(_.map(_.drop(boxes.toInt)))
          .updatedWith(finalStack.toInt)(toMove.zip(_).map(_ ++ _))

      case "" => stacks
      case _  => throw IllegalStateException(s"Unexpected input: $line")
    }

  private def readStacks(line: String, currentStack: Int, stacks: Stacks): Stacks =
    line.splitAt(4) match {
      case (s"[$id]$_", rest) =>
        readStacks(
          rest,
          currentStack + 1,
          stacks.updatedWith(currentStack)(_.map(id(0) :: _).orElse(Some(id(0) :: Nil)))
        )

      case (_, rest) =>
        if rest.isBlank() then stacks else readStacks(rest, currentStack + 1, stacks)
    }
