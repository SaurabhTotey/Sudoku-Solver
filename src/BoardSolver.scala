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
        def getBoardSquarePossibilities(board: Board): Seq[Seq[Seq[Int]]] = (0 until 9).map(i => (0 until 9).map(j => (0 until 9).filter(possibleValue => board.alterValue(i, j, possibleValue).isValid)))
        def innerSolveBoard(initialBoard: Board, possibleSolutions: Seq[Seq[Seq[Int]]]): Array[Board] = {
            if (!initialBoard.isValid || possibleSolutions.isEmpty) {
                throw new Exception()
            }
            if (initialBoard.isFilled) {
                return Array(initialBoard)
            }
            val squareLeastSolutions = (0 until 81).map(i => Array(i / 9, i % 9)).sortWith((a, b) => possibleSolutions(a(0))(a(1)).length < possibleSolutions(b(0))(b(1)).length).head
            val nextAnswerForSquare = possibleSolutions(squareLeastSolutions(0))(squareLeastSolutions(1)).head
            val nextBoard = initialBoard.alterValue(squareLeastSolutions(0), squareLeastSolutions(1), nextAnswerForSquare)
            try {
                innerSolveBoard(nextBoard, getBoardSquarePossibilities(nextBoard))
            } catch {
                case _: Exception =>
                    val newPossibleSolutions = (0 until 9).map(i => (0 until 9).map(j =>
                        if (i == squareLeastSolutions(0) && j == squareLeastSolutions(1)) {
                            possibleSolutions(i)(j).filter(element => element != nextAnswerForSquare)
                        } else {
                            possibleSolutions(i)(j)
                        }
                    ))
                    innerSolveBoard(initialBoard, newPossibleSolutions)
            }
        }
        innerSolveBoard(initialBoard, getBoardSquarePossibilities(initialBoard))
    }

}