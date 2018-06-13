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
        class SolutionFoundException(val foundSolution: Array[Board]) extends Exception
        if (initialBoard.isValid && initialBoard.isFilled) {
            return Array(initialBoard)
        }
        if (initialBoard == null || !initialBoard.isValid) {
            return null
        }
        val possibleSolutions = (0 until 9).map(i => (0 until 9).map(j => (1 to 9).filter(possibleValue => initialBoard.value(i, j) == 0 && initialBoard.alterValue(i, j, possibleValue).isValid)))
        try {
//            val valueToValidLocation = (0 until 9).map(value => (0 until 81).map(i => Array(i / 9, i % 9)).filter(locationForValue => initialBoard.alterValue(locationForValue(0), locationForValue(1), value + 1).isValid))
//            for ((locations, value) <- valueToValidLocation.zipWithIndex) {
//                for (location <- locations) {
//                    val otherLocations = locations.filter(otherLocation => !(otherLocation sameElements location))
//                    if (!otherLocations.exists(otherLocation => otherLocation(0) == location(0)) || !otherLocations.exists(otherLocation => otherLocation(1) == location(1)) || !otherLocations.exists(otherLocation => otherLocation(0) * 9 + otherLocation(1) == location(0) * 9 + location(1))) {
//                        throw new SolutionFoundException(Array(initialBoard) ++ solveBoard(initialBoard.alterValue(location(0), location(1), value + 1)))
//                    }
//                }
//            }
            for (solutionLocation <- (0 until 81).map(i => Array(i / 9, i % 9)).filter(a => possibleSolutions(a(0))(a(1)).nonEmpty).sortBy(a => possibleSolutions(a(0))(a(1)).length)) {
                for (solution <- possibleSolutions(solutionLocation(0))(solutionLocation(1))) {
                    val solutionPath = solveBoard(initialBoard.alterValue(solutionLocation(0), solutionLocation(1), solution))
                    if (solutionPath != null && solutionPath.last != null && solutionPath.last.isValid) {
                        throw new SolutionFoundException(Array(initialBoard) ++ solutionPath)
                    }
                }
            }
            null
        } catch {
            case solutionException: SolutionFoundException => solutionException.foundSolution
        }
    }

}