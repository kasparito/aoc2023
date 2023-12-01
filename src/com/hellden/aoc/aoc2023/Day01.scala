package com.hellden.aoc.aoc2023

object Day01 extends Day(1):

  override def part1: Long = // 54561

    def calibrationValue(s: String): Long =
      String.valueOf(s.find(_.isDigit).toArray ++ s.findLast(_.isDigit)).toLong

    inputLines.map(calibrationValue).sum

  override def part2: Long = // 54076

    val digits = IndexedSeq("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
                 ++ (0 to 9).map(_.toString)

    def findDigit(s: String): Option[Int] =
      digits.find(s.startsWith).map:
        case digit if digit(0).isDigit => digit.toInt
        case digit => digits.indexOf(digit)

    def calibrationValue(s: String): Long =
      val digits = s.indices.view.map(s.substring).flatMap(findDigit).toList
      (digits.head.toString + digits.last.toString).toLong

    inputLines.map(calibrationValue).sum
