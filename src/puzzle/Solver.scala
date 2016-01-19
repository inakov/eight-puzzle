package puzzle

import puzzle.Board.BoardConfiguration

import scala.io.StdIn

/**
 * Created by inakov on 16-1-19.
 */
object Solver extends App{

  val n: Int = 3
  val goal = goalBoard()
  
  val inputBlocks = Array.ofDim[Int](n, n)
  println("Enter board configuration:\n")

  for(i <- 0 until n)
    for(j <- 0 until n)
      inputBlocks(i)(j) = StdIn.readInt()
  
  val initialBoard = BoardConfiguration(inputBlocks, 0, Board.manhattan(goal.state, inputBlocks))

  object HeuristicOrdering extends  Ordering[BoardConfiguration]{
    override def compare(x: BoardConfiguration, y: BoardConfiguration): Int =
      (y.cost+y.distance) compare (x.cost+x.distance)
  }

  val openQueue = scala.collection.mutable.PriorityQueue.empty(HeuristicOrdering)
  var closedSet = Set.empty[BoardConfiguration]

  openQueue.enqueue(initialBoard)
  var finished = false
  while(openQueue.nonEmpty && !finished){
    val currentBoard = openQueue.dequeue()

    if(Board.isGoal(goal, currentBoard)){
      finished = true
      println(s"Solution in ${currentBoard.cost} moves.")
    }

    closedSet += currentBoard
    for(successor <- Board.generateSuccessors(currentBoard, goal))
      if(!closedSet.contains(successor)) openQueue.enqueue(successor)

  }

  def printBoard(boardConfiguration: BoardConfiguration): Unit ={
    for (i <- 0 until n) {
      for ( j <- 0 until n)
        print(" " + boardConfiguration.state(i)(j))

      println()
    }
  }

  def goalBoard(): BoardConfiguration = {
    var currentNumber: Int = 0

    val blocks = Array.ofDim[Int](n, n)
    for(i <- 0 until n)
      for(j <- 0 until n){
        blocks(i)(j) = currentNumber
        currentNumber += 1
      }

    BoardConfiguration(blocks, -1, 0)
  }

}
