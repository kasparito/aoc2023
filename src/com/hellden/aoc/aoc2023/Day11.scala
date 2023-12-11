package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.Day10.PipeField.*
import com.hellden.aoc.aoc2023.Direction.*

object Day11 extends Day(11):

  object ExpandingUniverse:

    private val space = inputLines
      .map(_.toIndexedSeq)
      .toIndexedSeq

    private val emptyRows =
      space
        .zipWithIndex
        .collect:
          case (row, index) if !row.contains('#') =>
            index

    private val emptyColumns =
      space
        .transpose
        .zipWithIndex
        .collect:
          case (column, index) if !column.contains('#') =>
            index

    private def calculateDistance(p1: Position, p2: Position, factor: Long): Long =
      val expandingRows = emptyRows.intersect(math.min(p1.y, p2.y) to math.max(p1.y, p2.y)).size
      val expandingColumns = emptyColumns.intersect(math.min(p1.x, p2.x) to math.max(p1.x, p2.x)).size
      (p1.x - p2.x).abs + (p1.y - p2.y).abs + expandingRows * (factor - 1) + expandingColumns * (factor - 1)

    def distances(factor: Long): Long =
      val galaxies = new Grid(space).find('#').toSeq
      val pairs = for
        g1 <- galaxies
        g2 <- galaxies
        if g1 != g2
      yield Set(g1, g2)
      pairs
        .toSet
        .view
        .map(_.toList)
        .collect:
          case List(g1, g2) => calculateDistance(g1, g2, factor)
        .sum

  override def part1: Long = // 10077850
    ExpandingUniverse.distances(2)

  override def part2: Long = // 504715068438
    ExpandingUniverse.distances(1_000_000)
