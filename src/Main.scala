import scala.io.Source

/**
  * Entry point of the program
  */
object Main extends App {
    val boardSize = 9
    val startingBoard = new Array[Array[Int]](boardSize)
    for ((letter, index) <- Source.fromFile("board.txt").mkString.filterNot(letter => letter.isWhitespace).zipWithIndex) {
        if (index % boardSize == 0) {
            startingBoard(index / boardSize) = new Array[Int](boardSize)
        }
        startingBoard(index / boardSize)(index % boardSize) = letter.asDigit
    }
    println(BoardSolver.solveBoard(new Board(startingBoard)) + "\n")
}
