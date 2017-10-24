package org.arajoa.listboard

class Move(var row: Int, var column: Int) {
  assert(1 to 3 contains row, "Incorrect row")
  assert(1 to 3 contains column, "Incorrect column")

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Move => this.hashCode() == that.hashCode()
    case _ => false
  }

  override def hashCode(): Int = row + 10 * column
}

object Move {
  implicit def moveToPosition(move: Move): Int = (move.row - 1) + ((move.column - 1) * 3)

  implicit def positionToMove(position: Int): Move = new Move(position % 3, (position / 3) + 1)
}
