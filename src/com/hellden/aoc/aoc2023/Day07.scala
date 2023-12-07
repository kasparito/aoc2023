package com.hellden.aoc.aoc2023

object Day07 extends Day(7):

  case class Hand(cards: String, bid: Long, sortValue: String)
  given Ordering[Hand] = Ordering.by(_.sortValue)

  def sortValue(cards: String, cardOrder: String, joker: Option[Char]): String =
    val orderedAmounts = cards.filterNot(joker.contains).groupBy(identity).values.map(_.length).toSeq.sorted.reverse
    val topCardAmount = cards.count(joker.contains) + orderedAmounts.headOption.getOrElse(0)
    val secondCardAmount = if orderedAmounts.size > 1 then orderedAmounts(1) else 0
    val typeValue = (topCardAmount, secondCardAmount) match
      case (3, 2) => 32
      case (2, 2) => 22
      case (x, _) => x * 10
    typeValue.toString + cards.map(card => ('A' + cardOrder.indexOf(card)).toChar)

  def winningsFor(cardOrder: String, joker: Option[Char] = None): Long =
    inputLines
      .map(_.split(' '))
      .collect { case Array(cards, bid) => Hand(cards, bid.toLong, sortValue(cards, cardOrder, joker)) }
      .sorted.zipWithIndex
      .map { (hand, index) => hand.bid * (index + 1L) }
      .sum

  override def part1: Long = // 254024898
    winningsFor(cardOrder = "23456789TJQKA")

  override def part2: Long = // 254115617
    winningsFor(cardOrder = "J23456789TQKA", joker = Some('J'))
