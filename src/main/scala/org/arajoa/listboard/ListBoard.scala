package org.arajoa.listboard

object WinningCombinations {
  val DIAG_1 = List(1, 0, 0, 0, 1, 0, 0, 0, 1)
  val DIAG_2 = List(0, 0, 1, 0, 1, 0, 1, 0, 0)

  val VER_1 = List(1, 0, 0, 1, 0, 0, 1, 0, 0)
  val VER_2 = List(0, 1, 0, 0, 1, 0, 0, 1, 0)
  val VER_3 = List(0, 0, 1, 0, 0, 1, 0, 0, 1)

  val HOR_1 = List(1, 1, 1, 0, 0, 0, 0, 0, 0)
  val HOR_2 = List(0, 0, 0, 1, 1, 1, 0, 0, 0)
  val HOR_3 = List(0, 0, 0, 0, 0, 0, 1, 1, 1)

  val ALL = List(DIAG_1, DIAG_2, VER_1, VER_2, VER_3, HOR_1, HOR_2, HOR_3)
}

object ListBoard {

  case class FailureReason(reason: String)

  type Board = List[Move]

  def createBoard(): Board = List()

  def move(board: Board, move: Move): Either[FailureReason, Board] = {
    if (board contains move)
      Left(FailureReason("Move Not Available"))
    else
      Right(board :+ move)
  }

  def checkWinner(board: Board): Option[Player] = {
    def checkWinner(board: Board, combinations: List[List[Int]]): Option[Player] = {
      if (combinations.isEmpty) None
      else if (combinations.head(board.last) == 0) {
        checkWinner(board, combinations.tail)
      } else {
        checkSolutionValue(board, combinations.head, 0, Player.FirstPlayer) match {
          case 3 => Some(Player.FirstPlayer)
          case -3 => Some(Player.SecondPlayer)
          case _ => checkWinner(board, combinations.tail)
        }
      }
    }

    def checkSolutionValue(board: Board, combination: List[Int], points: Int, player: Player): Int = {
      if (board.isEmpty) points
      else
        checkSolutionValue(board.tail, combination, points + (combination(board.head) * player.value), !player)
    }

    checkWinner(board, WinningCombinations.ALL)
  }

  def isDraw(board: Board): Boolean = board.length == 9

  def whoseTurn(board: Board): Player = board.length match {
    case even if even % 2 == 0 => Player.FirstPlayer
    case _ => Player.SecondPlayer
  }

  def printBoard(board: Board): Unit = {
    def printLegend(): Unit = {
      println(s"${Player.FirstPlayer}")
      println(s"${Player.SecondPlayer}")
    }

    def updateBoard(board: Board, boardToPrint: List[String], player: Player = Player.FirstPlayer): List[String] = {
      if (board.isEmpty) boardToPrint
      else
        updateBoard(board.tail, boardToPrint.updated(board.head, player.symbol), !player)
    }

    def boardToPrint = updateBoard(board, List.fill(9)("-"))

    printLegend()
    println("   | 1 | 2 | 3 |")
    println(s" 1 | ${boardToPrint(0)} | ${boardToPrint(1)} | ${boardToPrint(2)} |")
    println(s" 2 | ${boardToPrint(3)} | ${boardToPrint(4)} | ${boardToPrint(5)} |")
    println(s" 3 | ${boardToPrint(6)} | ${boardToPrint(7)} | ${boardToPrint(8)} |")
  }
}
