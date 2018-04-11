(ns sudoku-solver.core
  (:require [sudoku-solver.board :refer :all])
  (:require [clojure.string :refer :all]))

(defn -main []
  (def board (Board (clojure.string/split-lines (slurp "resources/board.txt"))))
  (println board)
  )
