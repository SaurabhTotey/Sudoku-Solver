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
        def innerSolveBoard(initialBoard: Board, possibleSolutions: Seq[Seq[Seq[Int]]], fallbackBoard: Board = null, fallbackPossibleSolutions: Seq[Seq[Seq[Int]]] = null): Array[Board] = {
            if (initialBoard.isFilled && initialBoard.isValid) {
                return Array(initialBoard)
            }
            if (!initialBoard.isValid || possibleSolutions.isEmpty) {
                return innerSolveBoard(fallbackBoard, fallbackPossibleSolutions)
            }
            val squareLeastSolutions = (0 until 81).map(i => Array(i / 9, i % 9)).minBy(a => possibleSolutions(a(0))(a(1)).length)
            val nextAnswerForSquare = possibleSolutions(squareLeastSolutions(0))(squareLeastSolutions(1)).head
            val nextBoard = initialBoard.alterValue(squareLeastSolutions(0), squareLeastSolutions(1), nextAnswerForSquare)
            val fallback = (0 until 9).map(i => (0 until 9).map(j =>
                if (i == squareLeastSolutions(0) && j == squareLeastSolutions(1)) {
                    possibleSolutions(i)(j).filter(element => element != nextAnswerForSquare)
                } else {
                    possibleSolutions(i)(j)
                }
            ))
            innerSolveBoard(nextBoard, getBoardSquarePossibilities(nextBoard), initialBoard, fallback)
        }
        innerSolveBoard(initialBoard, getBoardSquarePossibilities(initialBoard))
    }

}