package com.hellden.aoc.aoc2023

object Day04 extends Day(4):

  object Card:
    private val CardPattern = """Card \s*(\d+): ((?:\s*\d+)+) \| ((?:\s*\d+)+)""".r

    private def numbersFrom(s: String): Set[Long] = s
      .split("\\s+")
      .collect:
        case num if !num.isBlank => num.toLong
      .toSet

    def parse: String => Card =
      case CardPattern(num, winning, draw) =>
        Card(num.toInt, numbersFrom(draw).count(numbersFrom(winning)))

  case class Card(num: Int, wins: Int)

  val cards: IndexedSeq[Card] = inputLines.map(Card.parse).toIndexedSeq

  override def part1: Long = // 21088
    cards
      .map:
        case Card(_, 0) => 0L
        case Card(_, wins) => math.pow(2, wins - 1).toLong
      .sum

  override def part2: Long = // 6874754
    val (sum, _) = cards
      .foldLeft((0L, List.empty[Long])):
        case ((sum, Nil), card) =>
          (sum + 1, List.fill(card.wins)(1L))
        case ((sum, thisCard :: nextCards), card) =>
          val thisCardCount = thisCard + 1
          val updatedNextCards = nextCards
            .zipAll(List.fill(card.wins)(thisCardCount), 0L, 0L)
            .map: (x, y) =>
              x + y
          (sum + thisCardCount, updatedNextCards)
    sum
