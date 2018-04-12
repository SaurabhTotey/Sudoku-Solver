/**
  * Class that represents a board
  */
final class Board (private val values: Array[Array[Int]]) {

    /**
      * Gets the value at (n, m)
      * @param n the row of the value (0 is top)
      * @param m the column of the value (0 is left)
      * @return the value (n, m)
      */
    def value(n: Int, m: Int): Int = this.values(n)(m)

    /**
      * Gets what elements the board currently has
      * @return a list of the board's unique elements
      */
    def distinct: Array[Int] = this.values.flatten.distinct

    /**
      * Gets whether the board is valid
      * Is a superficial check, so it won't check that the board is still solvable, just whether it is currently fine
      * @return the validity of the board in its current state
      */
    def isValid: Boolean = {
        def isVectorValid(vec: Array[Int]): Boolean = {
            val strippedVector = vec.filterNot(elem => elem == 0)
            strippedVector.distinct.length == strippedVector.length
        }
        !(0 until 9).exists(i => !isVectorValid(row(i)) || !isVectorValid(column(i)))
    }

    /**
      * Returns whether the board is solved or not
      * @return whether the board is solved or not
      */
    def isSolved: Boolean = this.isValid && !this.distinct.contains(0)

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
      * Makes a board that is the same as this board, but with the new row
      * @param n row index
      * @param modified new row
      * @return new board with modified row
      */
    def alterRow(n: Int, modified: Array[Int]): Board = {
        var modifiedBoard = this.clone().asInstanceOf[Board]
        for ((value, index) <- modified.zipWithIndex) {
            modifiedBoard = modifiedBoard.alterValue(n, index, value)
        }
        modifiedBoard
    }

    /**
      * Makes a board that is the same as this board, but with the new column
      * @param n column index
      * @param modified new column
      * @return new board with modified column
      */
    def alterColumn(n: Int, modified: Array[Int]): Board = {
        var modifiedBoard: Board = this.clone().asInstanceOf[Board]
        for ((value, index) <- modified.zipWithIndex) {
            modifiedBoard = modifiedBoard.alterValue(index, n, value)
        }
        modifiedBoard
    }

    /**
      * Makes a board that is the same as this board, but with the new block
      * @param n row index
      * @param m column index
      * @param modified new block
      * @return new board with modified block
      */
    def alterBlock(n: Int, m: Int, modified: Board): Board = {
        var modifiedBoard = this.clone().asInstanceOf[Board]
        for (i <- 0 until 3) {
            for (j <- 0 until 3) {
                modifiedBoard = modifiedBoard.alterValue(n * 3 + i, m * 3 + j, modifiedBoard.value(i, j))
            }
        }
        modifiedBoard
    }

    /**
      * Gets the board as a pretty string
      * @return a string representation of the board
      */
    override def toString: String = {
        this.values.map(row => row.mkString(" ")).mkString("\n")
    }

}
