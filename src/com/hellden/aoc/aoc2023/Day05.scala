package com.hellden.aoc.aoc2023

import com.hellden.aoc.aoc2023.BinaryTree.Matchable

import scala.collection.immutable.NumericRange

object Day05 extends Day(5):

  private object Conversions:

    private case class Range(range: NumericRange[Long], delta: Long)

    private given Ordering[Range] = Ordering.by(_.range.start)
    private given Matchable[Range, Long, Long] with
      extension (r: Range)
        def key: Long = r.range.start
        def matching(num: Long): Option[Long] =
          Option.when(r.range.contains(num)):
            num + r.delta

    private def parse: List[String] => List[BinaryTree[Range]] =
      case _ :: _ :: lines =>
        val conversion = lines
          .takeWhile(_.headOption.exists(_.isDigit))
          .map:
            case s"$dest $src $len" =>
              val destStart = dest.toLong
              val sourceStart = src.toLong
              Range(sourceStart until sourceStart + len.toLong, destStart - sourceStart)
        BinaryTree(conversion) :: parse(lines.drop(conversion.size))
      case _ =>
        Nil

    private def convert(tree: BinaryTree[Range], num: Long): Long =
      tree.find(num).getOrElse(num)

    private val conversions = parse(inputLines.tail.toList)

    def convert(num: Long): Long =
      conversions.foldLeft(num): (num, conversion) =>
        convert(conversion, num)

  private val seeds =
    inputLines match
      case s"seeds: $seeds" +: maps =>
        seeds
          .split(" ")
          .map(_.toLong)

  override def part1: Long = // 57075758
    seeds.map(Conversions.convert).min

  override def part2: Long = // 31161857
    seeds
      .sliding(2, 2)
      .flatMap:
        case Array(start, length) =>
          start until start + length
      .map(Conversions.convert)
      .min
