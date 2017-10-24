package org.arajoa.listboard

import org.arajoa.listboard.ListBoard._

import scala.io.StdIn

object LetsPlay extends App {
  def play(board: Board): Option[Player] = {
    checkWinner(board) match {
      case Some(winner) => Some(winner)
      case _ if isDraw(board) => None
      case _ =>
        println()
        printBoard(board)
        println(s"Turn: ${whoseTurn(board).name}")
        print("Insert move (<row> <column>): ")
        val inputMove = readInput()

        move(board, inputMove) match {
          case Left(reason) =>
            println(s"Error with input: $reason")
            play(board)
          case Right(b) => play(b)
        }
    }
  }

  def readInput(): Move = {
    val input = StdIn.readLine().trim().split(" ")
    new Move(input(0).toInt, input(1).toInt)
  }

  def winningMessage(player: Player): Unit = {
    println()
    println("We have a winner!")
    println(s"All hail the mighty ${player.name}!!")
  }

  def losersMessage(): Unit = {
    println()
    println("Draw!! You pair of useless cunts!")
  }

  play(createBoard()) match {
    case Some(winner) => winningMessage(winner)
    case _ => losersMessage()
  }
}
