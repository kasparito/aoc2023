package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.Day03.EngineSchematic.*

object Day03 extends Day(3):

  object EngineSchematic:

    trait Part:
      def position: Position
      def length: Int
      def adjacent(position: Position): Boolean =
        (position.x - 1 to position.x + length).contains(position.x) &&
        (position.y - 1 to position.y + 1).contains(position.y)

    case class Symbol(symbol: Char, position: Position) extends Part:
      def length = 1

    case class PartNumber(num: Long, position: Position, length: Int) extends Part

    private def findParts(s: String, position: Position): List[Part] =
      s.headOption match
        case None =>
          Nil
        case Some('.') =>
          val drop = s.takeWhile(_ == '.')
          findParts(s.drop(drop.length), position.move(dx = drop.length))
        case Some(digit) if digit.isDigit =>
          val digits = s.takeWhile(_.isDigit)
          PartNumber(digits.toLong, position, digits.length) ::
          findParts(s.drop(digits.length), position.move(dx = digits.length))
        case Some(symbol) =>
          Symbol(symbol, position) :: findParts(s.drop(1), position.move(dx = 1))

    val parts: Seq[Part] = inputLines
      .zipWithIndex
      .flatMap: (line, y) =>
        findParts(line, Position(0, y))

  override def part1: Long = // 529618

    def hasAdjacentSymbol(partNumber: PartNumber): Boolean =
      parts
        .exists:
           case s: Symbol => partNumber.adjacent(s.position)
           case _ => false

    parts
      .collect:
        case partNumber: PartNumber if hasAdjacentSymbol(partNumber) =>
          partNumber.num
      .sum

  override def part2: Long = // 77509019

    def ratioFor(gearPosition: Position): Long =
      val adjacentPartNumbers = parts
        .collect:
          case partNumber: PartNumber if partNumber.adjacent(gearPosition) =>
            partNumber.num
      if adjacentPartNumbers.size > 1 then adjacentPartNumbers.product else 0

    parts
      .collect:
        case Symbol('*', position) =>
          ratioFor(position)
      .sum
