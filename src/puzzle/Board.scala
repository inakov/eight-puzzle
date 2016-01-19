package puzzle

/**
 * Created by inakov on 16-1-19.
 */
object Board {
  case class BoardConfiguration(state: Array[Array[Int]], cost: Int, distance: Int){

    override def equals(obj: Any) = obj match {
      case that: BoardConfiguration => isSame(this.state, that.state)
      case _ => false
    }

    def isSame(goalBoard: Array[Array[Int]], currentConfiguration: Array[Array[Int]]): Boolean = {
      for (i <- goalBoard.indices)
        if(!goalBoard(i).sameElements(currentConfiguration(i)))
          return false

      true
    }
  }


  def manhattan(goalBoard: Array[Array[Int]], currentConfiguration: Array[Array[Int]]): Int = {
    var distance: Int = 0

    def goalPosition(elem: Int): (Int, Int) = {
      val result = for {
        rowIndex <- goalBoard.indices
        if goalBoard(rowIndex).contains(elem)
      } yield (rowIndex, goalBoard(rowIndex).indexOf(elem))

      result.head
    }

    for(x <- currentConfiguration.indices)
      for(y <- currentConfiguration(x).indices){
        val (gx, gy) = goalPosition(currentConfiguration(x)(y))
        distance += Math.abs(x - gx) + Math.abs(y - gy)
      }

    distance
  }

  def isGoal(goalBoard: BoardConfiguration, currentConfiguration: BoardConfiguration): Boolean = {
    for (i <- goalBoard.state.indices)
      if(!goalBoard.state(i).sameElements(currentConfiguration.state(i)))
        return false

    true
  }

  def generateSuccessors(currentConfiguration: BoardConfiguration,
                         goalBoard: BoardConfiguration): List[BoardConfiguration] = {

    def emptyPosition(): (Int, Int) = {
      val result = for {
        rowIndex <- currentConfiguration.state.indices
        if currentConfiguration.state(rowIndex).contains(0)
      } yield (rowIndex, currentConfiguration.state(rowIndex).indexOf(0))

      result.head
    }
    var successors: List[BoardConfiguration] = Nil

    val (ex, ey) = emptyPosition()

    if(currentConfiguration.state.isDefinedAt(ex-1)){
      val (tx, ty) = (ex-1, ey)
      val successor = currentConfiguration.state.map(_.clone())
      successor(ex)(ey) = currentConfiguration.state(tx)(ty)
      successor(tx)(ty) = currentConfiguration.state(ex)(ey)
      val distance = manhattan(goalBoard.state, successor)
      successors = BoardConfiguration(successor, currentConfiguration.cost + 1, distance) :: successors
    }

    if(currentConfiguration.state.isDefinedAt(ex+1)){
      val (bx, by) = (ex+1, ey)
      val successor = currentConfiguration.state.map(_.clone())
      val bottomElement = currentConfiguration.state(bx)(by)
      val emptyElement = currentConfiguration.state(ex)(ey)
      successor(ex)(ey) = bottomElement
      successor(bx)(by) = emptyElement
      val distance = manhattan(goalBoard.state, successor)
      successors = BoardConfiguration(successor, currentConfiguration.cost + 1, distance) :: successors
    }

    if(currentConfiguration.state(ex).isDefinedAt(ey+1)){
      val (rx, ry) = (ex, ey+1)
      val successor = currentConfiguration.state.map(_.clone())
      successor(ex)(ey) = currentConfiguration.state(rx)(ry)
      successor(rx)(ry) = currentConfiguration.state(ex)(ey)
      val distance = manhattan(goalBoard.state, successor)
      successors = BoardConfiguration(successor, currentConfiguration.cost + 1, distance) :: successors
    }

    if(currentConfiguration.state(ex).isDefinedAt(ey-1)){
      val (lx, ly) = (ex, ey-1)
      val successor = currentConfiguration.state.map(_.clone())
      successor(ex)(ey) = currentConfiguration.state(lx)(ly)
      successor(lx)(ly) = currentConfiguration.state(ex)(ey)
      val distance = manhattan(goalBoard.state, successor)
      successors = BoardConfiguration(successor, currentConfiguration.cost + 1, distance) :: successors
    }

    successors
  }
}
