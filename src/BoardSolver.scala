/**
  * A namespace to separate out board solving logic from other parts or sections of the code
  */
object BoardSolver {

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
            val rowIndex = (0 until 9).find(i => initialBoard.row(i).count(num => num == 0) == 1).get
            val columnIndex = (0 until 9).find(i => initialBoard.value(rowIndex, i) == 0).get
            initialBoard.alterValue(rowIndex, columnIndex, (1 to 9).find(num => !initialBoard.row(rowIndex).contains(num)).get)
        } else if ((0 until 9).exists(i => initialBoard.column(i).count(num => num == 0) == 1)) {
            val columnIndex = (0 until 9).find(i => initialBoard.column(i).count(num => num == 0) == 1).get
            val rowIndex = (0 until 9).find(i => initialBoard.value(i, columnIndex) == 0).get
            initialBoard.alterValue(rowIndex, columnIndex, (1 to 9).find(num => !initialBoard.column(columnIndex).contains(num)).get)
        } else if((0 until 9).exists(i => initialBoard.block(i % 3, i / 3).values.flatten.count(num => num == 0) == 1)) {
            val outerBlockNumber = (0 until 9).find(i => initialBoard.block(i % 3, i / 3).values.flatten.count(num => num == 0) == 1).get
            val innerBlockNumber = (0 until 9).find(i => initialBoard.block(outerBlockNumber % 3, outerBlockNumber / 3).value(i % 3, i / 3) == 1).get
            initialBoard.alterValue((outerBlockNumber % 3) * 3 + innerBlockNumber % 3, outerBlockNumber / 3 * 3 + innerBlockNumber / 3, (1 to 9).find(num => !initialBoard.block(outerBlockNumber % 3, outerBlockNumber / 3).values.flatten.contains(num)).get)
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

}