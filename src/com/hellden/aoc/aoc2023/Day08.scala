package com.hellden.aoc.aoc2023

object Day08 extends Day(8):

  val instructions = LazyList.continually(inputLines.head).flatten
  val elementMap = inputLines
    .drop(2)
    .collect:
      case s"$element = ($left, $right)" =>
        element -> (left, right)
    .toMap

  def nextElement(element: String, direction: Char): String =
    val (left, right) = elementMap(element)
    direction match
      case 'L' => left
      case 'R' => right

  override def part1: Long = // 20513
    instructions
      .scanLeft("AAA")(nextElement)
      .zipWithIndex
      .collectFirst:
        case ("ZZZ", step) => step
      .get

  override def part2: BigInt = // 15995167053923
    def steps(element: String): Long =
      instructions
        .scanLeft(element)(nextElement)
        .zipWithIndex
        .collectFirst: 
          case (element, step) if element.last == 'Z' => step
        .get
    def lcm(x: BigInt, y: BigInt): BigInt = x * y / x.gcd(y)
    elementMap
      .keys
      .collect { case element if element.last == 'A' => steps(element) }
      .map(BigInt.apply)
      .fold(BigInt(1))(lcm)
