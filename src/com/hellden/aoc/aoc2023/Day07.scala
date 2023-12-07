package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.Day07.Type._

object Day07 extends Day(7):

  enum Type:
    case HighCard
    case OnePair
    case TwoPair
    case ThreeOfKind
    case FullHouse
    case FourOfKind
    case FiveOfKind

  object Type:
    def typeFor(cardAmounts: Map[Char, Int], jokers: Int = 0): Type =
      val orderedAmounts = cardAmounts.values.toSeq.sorted.reverse
      val top = jokers + orderedAmounts.headOption.getOrElse(0)
      val second = if orderedAmounts.size > 1 then orderedAmounts(1) else 0
      (top, second) match
        case (5, _) => FiveOfKind
        case (4, _) => FourOfKind
        case (3, 2) => FullHouse
        case (3, _) => ThreeOfKind
        case (2, 2) => TwoPair
        case (2, _) => OnePair
        case _ => HighCard

  case class Hand(cards: String, bid: Long, `type`: Type)

  def winnings(cardOrder: String, typeFor: String => Type): Long =

    given Ordering[Hand] with
      override def compare(x: Hand, y: Hand): Int =
        x.`type`.ordinal - y.`type`.ordinal match
          case 0 =>
            x.cards.zip(y.cards)
             .map { (xc, yc) => cardOrder.indexOf(xc) - cardOrder.indexOf(yc) }
             .filter(_ != 0)
             .head
          case cmp => cmp

    inputLines
      .map(_.split(' '))
      .collect { case Array(cards, bid) => Hand(cards, bid.toLong, typeFor(cards)) }
      .sorted.zipWithIndex
      .map { (hand, index) => hand.bid * (index + 1L) }
      .sum

  def cardAmounts(cards: String): Map[Char, Int] =
    cards.groupBy(identity).view.mapValues(_.length).toMap

  override def part1: Long = // 254024898
    winnings("23456789TJQKA", cards => typeFor(cardAmounts(cards)))

  override def part2: Long = // 254115617
    winnings("J23456789TQKA", cards => typeFor(cardAmounts(cards.filter(_ != 'J')), jokers = cards.count(_ == 'J')))
