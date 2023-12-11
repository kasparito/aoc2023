package com.hellden.aoc

package object aoc2023:

  case class Position(x: Int, y: Int):
    def move(dx: Int = 0, dy: Int = 0): Position =
      Position(x + dx, y + dy)

  enum Direction(val dx: Int, val dy: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)

    def opposite = this match
      case Up => Down
      case Down => Up
      case Left => Right
      case Right => Left
    
    def move(position: Position): Position =
      Position(position.x + dx, position.y + dy)

  class Grid[T](grid: IndexedSeq[IndexedSeq[T]]):

    val horizontalBounds: Range = 0 until grid.map(_.length).max
    val verticalBounds: Range = grid.indices

    def isWithinBounds(x: Int, y: Int): Boolean =
      horizontalBounds.contains(x) && verticalBounds.contains(y)
    
    def move(position: Position, direction: Direction, steps: Int = 1): Option[(Position, T)] =
      val x = position.x + direction.dx * steps
      val y = position.y + direction.dy * steps
      Option.when(isWithinBounds(x, y)):
        (Position(x, y), grid(y)(x))

    def valueFor(position: Position): Option[T] =
      Option.when(isWithinBounds(position.x, position.y)):
        grid(position.y)(position.x)

    def find(v: T): Iterable[Position] =
      for
        x <- horizontalBounds
        y <- verticalBounds
        position = Position(x, y)
        value <- valueFor(position)
        if value == v
      yield
        position
