/**
  * Class that represents a board
  */
private class Board (val values: Array[Array[Int]]) {

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
}
