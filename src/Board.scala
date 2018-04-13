/**
  * Class that represents a Sudoku board
  */
final class Board (val values: Array[Array[Int]]) {

    /**
      * Gets the value at (n, m)
      * @param n the row of the value (0 is top)
      * @param m the column of the value (0 is left)
      * @return the value (n, m)
      */
    def value(n: Int, m: Int): Int = this.values(n)(m)

    /**
      * Gets whether the board is valid
      * Is a superficial check, so it won't check that the board is still solvable, just whether it is currently fine
      * @return the validity of the board in its current state
      */
    def isValid: Boolean = {
        def isVectorValid(vec: Array[Int]): Boolean = {
            val strippedVector = vec.filter(_ != 0)
            strippedVector.distinct.length == strippedVector.length
        }
        !(0 until 9).exists(i => !isVectorValid(row(i)) || !isVectorValid(column(i)) || !isVectorValid(block(i % 3, i / 3).values.flatten))
    }

    /**
      * Returns whether the board is filled or not: does not take into account whether it is correct
      * @return whether the board is filled or not
      */
    def isFilled: Boolean = !this.values.flatten.contains(0)

    /**
      * Gets the nth row of the board
      * @param n the nth row of the board
      */
    def row(n: Int): Array[Int] = values(n)

    /**
      * Gets the nth column of the column
      * @param n the nth column of the board
      */
    def column(n: Int): Array[Int] = values.map(row => row(n))

    /**
      * Gets the block or the subsquare at (n, m)
      * @param n row of the block (0 is top)
      * @param m column of the block (0 is left)
      * @return smaller board that is the specified block
      */
    def block(n: Int, m: Int): Board = {
        val rowStart = n * 3
        val colStart = m * 3
        new Board(values.slice(rowStart, rowStart + 3).map(row => row.slice(colStart, colStart + 3)))
    }

    /**
      * Makes a board that is the same as this board, with the new value
      * @param n row index
      * @param m column index
      * @param modified new value
      * @return new board with modified value
      */
    def alterValue(n: Int, m: Int, modified: Int): Board = {
        val clone = this.values.clone()
        clone(n)(m) = modified
        new Board(clone)
    }

    /**
      * Gets the board as a pretty string
      * @return a string representation of the board
      */
    override def toString: String = {
        this.values.map(row => row.mkString(" ")).mkString("\n")
    }

}
