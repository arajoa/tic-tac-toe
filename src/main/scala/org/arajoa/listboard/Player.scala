package org.arajoa.listboard

trait Player {
  def value: Int

  def name: String

  def symbol: String

  def unary_! : Player

  override def toString: String = s"$name: $symbol"
}

object Player {

  object FirstPlayer extends Player {
    override def value: Int = 1

    override def symbol: String = "X"

    override def name: String = "First Player"

    override def unary_! : Player = SecondPlayer
  }

  object SecondPlayer extends Player {
    override def value: Int = -1

    override def symbol: String = "O"

    override def name: String = "Second Player"

    override def unary_! : Player = FirstPlayer
  }

}
