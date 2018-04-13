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
        if (!initialBoard.isValid) {
            var filledBoard = initialBoard
            for (i <- 0 until 9) {
                for (j <- 0 until 9) {
                    if (filledBoard.value(i, j) == 0) {
                        filledBoard = filledBoard.alterValue(i, j, 10)
                    }
                }
            }
            return Array(filledBoard)
        }
        for (i <- 0 until 9) {
            for (j <- 0 until 9) {
                for (possibleAnswer <- (0 until 9).filter(possibleValue => initialBoard.alterValue(i, j, possibleValue).isValid)) {
                    val branchedSolution = solveBoard(initialBoard.alterValue(i, j, possibleAnswer))
                    if (branchedSolution(branchedSolution.length - 1).isValid) {
                        return Array(initialBoard) ++ branchedSolution
                    }
                }
            }
        }
        null
    }

}