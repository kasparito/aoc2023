package com.hellden.aoc.aoc2023

import Direction._
import com.hellden.aoc.aoc2023.Day10.PipeField._

object Day10 extends Day(10):

  override val input: String = """...........
                                 |.S-------7.
                                 |.|F-----7|.
                                 |.||.....||.
                                 |.||.....||.
                                 |.|L-7.F-J|.
                                 |.|..|.|..|.
                                 |.L--J.L--J.
                                 |...........
                                 |""".stripMargin

  object PipeField:

    enum Pipe(val char: Char, val directions: Set[Direction]):
      case Empty extends Pipe('.', Set.empty)
      case LeftRight extends Pipe('-', Set(Left, Right))
      case LeftDown extends Pipe('7', Set(Left, Down))
      case RightDown extends Pipe('F',  Set(Right, Down))
      case UpDown extends Pipe('|', Set(Up, Down))
      case UpLeft extends Pipe('J', Set(Up, Left))
      case UpRight extends Pipe('L', Set(Up, Right))

    object Pipe:
      def unapply(char: Char): Option[Set[Direction]] =
        Pipe.values.collectFirst:
          case pipe if pipe.char == char => pipe.directions

    val grid = new Grid[Char](inputLines.map(_.toIndexedSeq).toIndexedSeq)

    val start = grid.find('S').head
    val initialDirections =
      val dirs = Direction
        .values
        .flatMap: direction =>
          grid.move(start, direction).collect:
            case (_, Pipe(directions)) if directions.contains(direction.opposite) =>
              direction
      (dirs(0), dirs(1))

  override def part1: Long = // ???

    def path(direction: Direction) =
      LazyList.iterate((start, direction)): (previousPosition, direction) =>
        grid
          .move(previousPosition, direction)
          .flatMap:
            case (nextPosition, Pipe(directions)) =>
              directions.collectFirst:
                case nextDirection if nextDirection.opposite != direction =>
                  (nextPosition, nextDirection)
          .head

    path(initialDirections._1)
      .zip(path(initialDirections._2))
      .zipWithIndex
      .drop(1)
      .collectFirst:
        case (((p1, d1), (p2, d2)), steps) if p1 == p2 => steps
      .get
