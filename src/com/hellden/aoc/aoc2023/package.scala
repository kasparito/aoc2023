package com.hellden.aoc

package object aoc2023 {

  case class Position(x: Int, y: Int)

  enum Direction(val dx: Int, val dy: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)

    def move(position: Position): Position =
      Position(position.x + dx, position.y + dy)

  class Grid[T](grid: IndexedSeq[IndexedSeq[T]]):

    val horizontalBounds: Range = 0 until grid.map(_.length).max
    val verticalBounds: Range = grid.indices

    def move(position: Position, direction: Direction): Option[Position] =
      val x = position.x + direction.dx
      val y = position.y + direction.dy
      Option.when(horizontalBounds.contains(x) && verticalBounds.contains(y)) {
                                                                                Position(x, y)
                                                                              }

    def valueFor(position: Position): T =
      grid(position.y)(position.x)

    def find(v: T): Iterable[Position] =
      for
        x <- horizontalBounds
        y <- verticalBounds
        position = Position(x, y)
        if valueFor(position) == v
      yield
        position
}
