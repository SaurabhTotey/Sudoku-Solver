import org.sat4j.core.VecInt
import org.sat4j.minisat.SolverFactory
import org.sat4j.specs.IVecInt

/**
  * A namespace to separate out board solving logic from other parts or sections of the code
  */
object BoardSolver {

    //The SAT solver that handles solving the Sudoku board
    private val solver = SolverFactory.newDefault()

    //Takes in a row, column, and value and returns a unique value for a solver variable
    private def createSolverValue(row: Int, col: Int, value: Int): Int = row * 81 + col * 9 + value + 1

    /**
      * Method that initializes the SAT solver to default conditions for Sudoku
      */
    private def initSolverForSudoku(): Unit = {
        this.solver.reset()
        //Registers variables for each possible value on each board space
        (0 until 9).foreach(row => (0 until 9).foreach(col => (0 until 9).foreach(value => this.solver.newVar(row * 81 + col * 9 + value))))
        //Takes in an integer vector and sets it so that only one of each of those values are allowed in the solver
        def setClauseForExactlyOneTrue(vector: IVecInt): Unit = {
            this.solver.addClause(vector)
            this.solver.addExactly(vector, 1)
        }
        //Defines in the solver that no row should contain duplicate numbers
        (0 until 9).foreach(row => (0 until 9).foreach(value => {
            val literals = new VecInt()
            (0 until 9).foreach(col => literals.push(this.createSolverValue(row, col, value)))
            setClauseForExactlyOneTrue(literals)
        }))
        //Defines in the solver that no row should contain duplicate numbers
        (0 until 9).foreach(col => (0 until 9).foreach(value => {
            val literals = new VecInt()
            (0 until 9).foreach(row => literals.push(this.createSolverValue(row, col, value)))
            setClauseForExactlyOneTrue(literals)
        }))
        //Defines in the solver that no single box should contain duplicate numbers
        (0 until 9).foreach(row => (0 until 9).foreach(col => {
            val literals = new VecInt()
            (0 until 9).foreach(value => literals.push(this.createSolverValue(row, col, value)))
            setClauseForExactlyOneTrue(literals)
        }))
        //Defines in the solver that no sub-block should contain duplicate numbers
        (0 until 3).foreach(bigRow => (0 until 3).foreach(bigCol => (0 until 9).foreach(value => {
            val literals = new VecInt()
            (0 until 3).foreach(innerRow => (0 until 3).foreach(innerCol => literals.push(this.createSolverValue(bigRow * 3 + innerRow, bigCol * 3 + innerCol, value))))
            setClauseForExactlyOneTrue(literals)
        })))
    }

    /**
      * Takes an input board and returns a solution board to that board
      * @param initialBoard the initial starting board to solve
      * @return a solved version of the inputted board
      */
    def solveBoard(initialBoard: Board): Board = {
        this.initSolverForSudoku()
        //Enters the board into the solver
        (0 until 9).foreach(row => (0 until 9).filter(col => initialBoard.value(row, col) != 0).foreach(col => {
            val value = new VecInt()
            value.push(this.createSolverValue(row, col, initialBoard.value(row, col) - 1))
            solver.addClause(value)
        }))
        //Gets the board from the solver
        this.solver.findModel()
        var returnBoard = initialBoard
        (0 until 9).foreach(row => (0 until 9).foreach(col => returnBoard = returnBoard.alterValue(row, col, (0 until 9).find(value => this.solver.model(createSolverValue(row, col, value))).get + 1)))
        returnBoard
    }

}