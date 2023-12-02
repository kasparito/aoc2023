package com.hellden.aoc.aoc2023

object Day02 extends Day(2):

  enum Color:
    case red
    case green
    case blue

  case class Game(id: Int, sets: List[Map[Color, Int]])

  object Game:
    private val Pattern = """Game (\d+): (.*)""".r
    private val SetPattern = """(\d+) (\w+)""".r

    def parse(s: String): Game =
      s match
        case Game.Pattern(id, sets) =>
          Game(id.toInt, sets.split("; ").map(parseSet).toList)

    private def parseSet(s: String): Map[Color, Int] = s
      .split(", ")
      .map:
        case SetPattern(num, color) =>
          Color.valueOf(color) -> num.toInt
      .toMap

  private val games = inputLines.map(Game.parse)

  override def part1: Long = // 2476
    import Color._

    val cubes = Map(
      red -> 12,
      green -> 13,
      blue -> 14
    )

    def possible(game: Game): Boolean =
      Color.values.forall: color =>
        game.sets.flatMap(_.get(color)).max <= cubes(color)

    games
      .collect:
        case game if possible(game) => game.id
      .sum

  override def part2: Long = // 54911
    def minimalSet(game: Game): Map[Color, Int] =
      Color.values.map(color => color -> game.sets.flatMap(_.get(color)).max).toMap

    def power(set: Map[Color, Int]): Long =
      set.values.product

    games
      .map(minimalSet)
      .map(power)
      .sum
