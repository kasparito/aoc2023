package com.hellden.aoc.aoc2023

object Day06 extends Day(6):

  private case class Race(time: Long, dist: Long)

  private def waysToWin(race: Race): Long =
    (0L to race.time).count: holdTime =>
      holdTime * (race.time - holdTime) > race.dist

  override def part1: Long = // 2756160
    List(
      Race(48, 296),
      Race(93, 1928),
      Race(85, 1236),
      Race(95, 1391)).map(waysToWin).product

  override def part2: Long = // 34788142
    waysToWin(Race(48938595L, 296192812361391L))
