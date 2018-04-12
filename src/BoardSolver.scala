/**
  * Solves the given board and returns a path to the correct solution
  * Solves the board by getting a board with all 0s filled with different valid numbers
  * @param initialBoard the initial starting board to solve
  * @return an array of boards that shows each step taken to get to the final finished solved board
  */
def solveBoard(initialBoard: Board): Array[Board] = {
    if (initialBoard.isFilled) {
        return Array(initialBoard)
    }
    var backtrackVal = 0
    var backtrackR = -1
    var backtrackC = -1
    var currentBoard: Board = if ((0 until 9).exists(i => initialBoard.row(i).count(num => num == 0) == 1)) {
        val rowIndex = (0 until 9).filter(i => initialBoard.row(i).count(num => num == 0) == 1)(0)
        val columnIndex = (0 until 9).filter(i => initialBoard.value(rowIndex, i) == 0)(0)
        initialBoard.alterValue(rowIndex, columnIndex, (1 to 9).filterNot(num => initialBoard.row(rowIndex).contains(num))(0))
    } else if ((0 until 9).exists(i => initialBoard.column(i).count(num => num == 0) == 1)) {
        val columnIndex = (0 until 9).filter(i => initialBoard.column(i).count(num => num == 0) == 1)(0)
        val rowIndex = (0 until 9).filter(i => initialBoard.value(i, columnIndex) == 0)(0)
        initialBoard.alterValue(rowIndex, columnIndex, (1 to 9).filterNot(num => initialBoard.column(columnIndex).contains(num))(0))
    } else {
        backtrackVal += 1
        //TODO: stuff with backtracking
        null
    }
    if (backtrackVal != 0) {
        //TODO: stuff with backtracking
        null
    } else {
        Array(initialBoard) ++ solveBoard(currentBoard)
    }
}