package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.Day03.EngineSchematic._

object Day03 extends Day(3):

  override def part1: Long = // 529618
    val nonSymbols = "0123456789.".toSet
    val grid = new Grid[Char](inputLines.map(_.toIndexedSeq).toIndexedSeq)

    def isSymbol(c: Char): Boolean = !nonSymbols(c)

    def isPartNumber(s: String, position: Position): Boolean =
      grid.valueFor(position.move(dx = -1)).exists(isSymbol) ||
      grid.valueFor(position.move(dx = s.length)).exists(isSymbol) ||
      (-1 to s.length).flatMap(dx => grid.valueFor(position.move(dx = dx, dy = -1))).exists(isSymbol) ||
      (-1 to s.length).flatMap(dx => grid.valueFor(position.move(dx = dx, dy = 1))).exists(isSymbol)

    def sum(s: String, position: Position): Long =
      if s.isEmpty then 0 else
        val drop = s.takeWhile(x => !x.isDigit)
        val digits = s.drop(drop.length).takeWhile(_.isDigit)
        val partNumber = if isPartNumber(digits, position.move(dx = drop.length)) then digits.toLong else 0
        val offset = drop.length + digits.length
        partNumber + sum(s.drop(offset), position.move(dx = offset))

    inputLines
      .view
      .zipWithIndex
      .map: (line, y) =>
        sum(line, Position(0, y))
      .sum

  object EngineSchematic:

    trait Part:
      def position: Position
      def length: Int

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

    def adjacent(position: Position, part: Part): Boolean =
      (part.position.x - 1 to part.position.x + part.length).contains(position.x) &&
      (part.position.y - 1 to part.position.y + 1).contains(position.y)

    val parts: Seq[Part] = inputLines
      .zipWithIndex
      .flatMap: (line, y) =>
        findParts(line, Position(0, y))

  override def part2: Long = // 77509019

    def ratioFor(gearPosition: Position): Long =
      val partNumbers = EngineSchematic
        .parts
        .collect:
          case partNumber: PartNumber if adjacent(gearPosition, partNumber) =>
            partNumber.num
      if partNumbers.size > 1 then partNumbers.product else 0

    EngineSchematic
      .parts
      .collect:
        case Symbol('*', position) =>
          ratioFor(position)
      .sum
