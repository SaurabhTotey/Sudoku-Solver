/**
  * Class that represents a board
  */
final class Board (val values: Array[Array[Int]]) {

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
        null //TODO:
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
        null //TODO:
    }

    /**
      * Makes a board that is the same as this board, but with the new column
      * @param n column index
      * @param modified new column
      * @return new board with modified column
      */
    def alterColumn(n: Int, modified: Array[Int]): Board = {
        null //TODO:
    }

    /**
      * Makes a board that is the same as this board, but with the new block
      * @param n row index
      * @param m column index
      * @param modified new block
      * @return new board with modified block
      */
    def alterBlock(n: Int, m: Int, modified: Board): Board = {
        null //TODO:
    }

}
