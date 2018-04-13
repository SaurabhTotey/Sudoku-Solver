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
        val possibleSolutions = (0 until 9).map(_ => (0 until 9).map(_ => new ArrayBuffer[Int]()))
        for (i <- 0 until 9) {
            for (j <- 0 until 9) {
                val list = possibleSolutions(i)(j)
                for (value <- (0 until 9).filter(possibleValue => initialBoard.alterValue(i, j, possibleValue).isValid)) {
                    list.append(value)
                }
                if (list.length == 1) {
                    println("a")
                    return Array(initialBoard) ++ solveBoard(initialBoard.alterValue(i, j, list(0)))
                }
            }
        }
        for (i <- 0 until 9) {
            for (j <- 0 until 9) {
                val list = possibleSolutions(i)(j)
                for (possibleAnswer <- list) {
                    val branchedSolution = solveBoard(initialBoard.alterValue(i, j, possibleAnswer))
                    if (branchedSolution != null && branchedSolution.length != 0) {
                        val solution = branchedSolution(branchedSolution.length - 1)
                        if (solution.isValid) {
                            println("b")
                            return Array(initialBoard) ++ branchedSolution
                        }
                    }
                }
            }
        }
        null
    }

}