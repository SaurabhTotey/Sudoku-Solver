import scala.io.Source

/**
  * Entry point of the program
  */
object Main extends App {
    println(Source.fromFile("res/board.txt").mkString)
}