package com.hellden.aoc.aoc2023

import scala.annotation.tailrec
import scala.collection.immutable.NumericRange

object Day05 extends Day(5):

  val input1: String = """seeds: 79 14 55 13
                                 |
                                 |seed-to-soil map:
                                 |50 98 2
                                 |52 50 48
                                 |
                                 |soil-to-fertilizer map:
                                 |0 15 37
                                 |37 52 2
                                 |39 0 15
                                 |
                                 |fertilizer-to-water map:
                                 |49 53 8
                                 |0 11 42
                                 |42 0 7
                                 |57 7 4
                                 |
                                 |water-to-light map:
                                 |88 18 7
                                 |18 25 70
                                 |
                                 |light-to-temperature map:
                                 |45 77 23
                                 |81 45 19
                                 |68 64 13
                                 |
                                 |temperature-to-humidity map:
                                 |0 69 1
                                 |1 0 69
                                 |
                                 |humidity-to-location map:
                                 |60 56 37
                                 |56 93 4
                                 |""".stripMargin

  private object Conversions:

    trait Tree:
      def start: Long
      def end: Long
      def depth: Int
    case class Leaf(range: NumericRange[Long], delta: Long) extends Tree:
      def start: Long = range.start
      def end: Long = range.end - 1
      def depth: Int = 1
    case class Branch(left: Tree, right: Tree) extends Tree:
      val start: Long = left.start
      val end: Long = right.end
      val depth: Int = math.max(left.depth, right.depth) + 1

    private def merge: (Tree, Tree) => Tree =
      case (l1: Leaf, l2: Leaf) if l1.start < l2.start =>
        Branch(l1, l2)
      case (l1: Leaf, l2: Leaf) =>
        Branch(l2, l1)
      case (Branch(b1, b2), t) if b2.start < t.start =>
        Branch(b1, merge(b2, t))
      case (Branch(b1, b2), t) =>
        Branch(merge(b1, t), b2)
      case (t, Branch(b1, b2)) if b1.start < t.start =>
        Branch(b1, merge(b2, t))
      case (t, Branch(b1, b2)) =>
        Branch(merge(b1, t), b2)

    private def parse: List[String] => List[Tree] =
      case _ :: _ :: lines =>
        val conversion = lines
          .takeWhile(_.headOption.exists(_.isDigit))
          .map:
            case s"$dest $src $len" =>
              val destStart = dest.toLong
              val sourceStart = src.toLong
              Leaf(sourceStart until sourceStart + len.toLong, destStart - sourceStart)
        conversion.reduce(merge) :: parse(lines.drop(conversion.size))
      case _ =>
        Nil

    @tailrec
    private def convert(tree: Tree, num: Long): Long =
      tree match
        case Leaf(range, delta) if range.contains(num) => num + delta
        case Branch(t1, _) if t1.end >= num => convert(t1, num)
        case Branch(_, t2) if t2.start <= num => convert(t2, num)
        case _ => num

    private val conversions = parse(inputLines.tail.toList)

    def convert(num: Long): Long =
      conversions.foldLeft(num): (num, conversion) =>
        convert(conversion, num)

  override def part1: Long = // 57075758
    inputLines match
      case s"seeds: $seeds" +: maps =>
        seeds
          .split(" ")
          .map(_.toLong)
          .map(Conversions.convert)
          .min

  override def part2: Long = // 31161857
    inputLines match
      case s"seeds: $seeds" +: maps =>
        seeds
          .split(" ")
          .map(_.toLong)
          .sliding(2, 2)
          .flatMap:
            case Array(start, length) =>
              start until start + length
          .map(Conversions.convert)
          .min
