import scala.collection.mutable.ArrayBuffer

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
        val possibleSolutions = initialBoard.values.map(row => row.map(_ => new ArrayBuffer[Int]()))
        for ((row, i) <- possibleSolutions.zipWithIndex) {
            for ((list, j) <- row.zipWithIndex) {
                for (value <- (0 until 9).filter(possibleValue => initialBoard.alterValue(i, j, possibleValue).isValid)) {
                    list.append(value)
                }
                if (list.length == 1) {
                    return Array(initialBoard) ++ solveBoard(initialBoard.alterValue(i, j, list(0)))
                } else {
                    for (possibleAnswer <- list) {
                        val branchedSolution = solveBoard(initialBoard.alterValue(i, j, possibleAnswer))
                        if (branchedSolution(branchedSolution.length - 1).isValid) {
                            return Array(initialBoard) ++ branchedSolution
                        }
                    }
                }
            }
        }
        null
    }

}