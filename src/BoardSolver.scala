import scala.util.control.Breaks._

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
        var solutionPath = Array(initialBoard)
        if (initialBoard.isFilled) {
            return Array(initialBoard)
        }
        if (!initialBoard.isValid) {
            return null
        }
        val possibleSolutions = (0 until 9).map(i => (0 until 9).map(j => (0 until 9).filter(possibleValue => initialBoard.alterValue(i, j, possibleValue).isValid)))
        breakable{
            for (solutionLocation <- (0 until 81).map(i => Array(i / 9, i % 9)).sortWith((a, b) => possibleSolutions(a(0))(a(1)).length < possibleSolutions(b(0))(b(1)).length)) {
                val answerCandidates = possibleSolutions(solutionLocation(0))(solutionLocation(1))
                if (answerCandidates.length == 1) {
                    solutionPath = solutionPath ++ solveBoard(initialBoard.alterValue(solutionLocation(0), solutionLocation(1), answerCandidates(0)))
                    break()
                } else {
                    for (candidate <- answerCandidates) {
                        val candidateSolutionPath = solveBoard(initialBoard.alterValue(solutionLocation(0), solutionLocation(1), candidate))
                        if (candidateSolutionPath != null && candidateSolutionPath.last != null && candidateSolutionPath.last.isValid) {
                            solutionPath = solutionPath ++ candidateSolutionPath
                            break()
                        }
                    }
                }
            }
        }
        solutionPath
    }

}