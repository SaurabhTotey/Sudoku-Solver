(ns sudoku-solver.board)

(def boardSize 9)

(defn Board [& boardAsRows]
  {
   :board boardAsRows
   :row (defn row [rowNumber] get boardAsRows rowNumber)
   :column (defn column [columnNumber] map (fn [row] get row columnNumber) boardAsRows)})