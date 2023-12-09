package com.hellden.aoc.aoc2023

object Day09 extends Day(9):

  def parse(line: String): List[Long] =
    line.split(' ').map(_.toLong).toList

  def extrapolate(nums: List[Long], reverse: Boolean = false): Long =
    def neg(num: Long): Long = if reverse then -num else num
    if nums.forall(_ == 0) then
      0
    else
      nums.last + neg(extrapolate(nums.sliding(2).map { case List(x, y) => neg(y - x) }.toList))

  override def part1: Long = // 2174807968
    inputLines.map(parse).map(extrapolate(_)).sum

  override def part2: Long = // 1208
    inputLines.map(parse).map(_.reverse).map(extrapolate(_, reverse = true)).sum
