/**
 * Conway's game of life - my solution
 * 2015 William Narmontas "Scala William"
 * https://www.scalawilliam.com/
 **/
sealed trait Cell {
  def character: String
}

case object AliveCell extends Cell {
  val character = "*"
}

case object DeadCell extends Cell {
  val character = "-"
}

object Cell {
  def unapply(str: String): Option[Cell] =
    PartialFunction.condOpt(str) {
      case s if s == AliveCell.character => AliveCell
      case s if s == DeadCell.character => DeadCell
    }
}

case class Position(x: Int, y: Int) {

  private def neighbourTo(other: Position): Boolean =
    this != other && (Math.abs(x - other.x) <= 1) && (Math.abs(y - other.y) <= 1)

  private def neighbours(state: State): Set[Position] =
    state.cells.collect {
      case (other, _) if neighbourTo(other) => other
    }.toSet

  def numberOfLiveNeighbours(inState: State): Int =
    neighbours(inState).toList.map(inState.cells).count(_ == AliveCell)
}

case class State(cells: Map[Position, Cell]) {
  state =>

  private implicit class positionNeighbours(position: Position) {
    def aliveNeighbourCount = position.numberOfLiveNeighbours(inState = state)
  }

  def next: State = {
    val updatedCells = cells.collect {
      case (position, AliveCell) if position.aliveNeighbourCount < 2 =>
        position -> DeadCell
      case (position, AliveCell) if position.aliveNeighbourCount > 3 =>
        position -> DeadCell
      case (position, DeadCell) if position.aliveNeighbourCount == 3 =>
        position -> AliveCell
    }
    copy(cells = cells ++ updatedCells)
  }

  private def numRows = cells.keySet.map(_.y).max

  private def numCols = cells.keySet.map(_.x).max

  def render: String = {

    def topLine = s"""╔${List.fill(numCols)("=").mkString}╗"""
    def bottomLine = s"""╚${List.fill(numCols)("=").mkString}╝"""

    case class PositionCell(position: Position, cell: Cell)

    val lines = for {
      y <- 1 to numRows
      cellsInLine = cells.collect {
        case (position @ Position(x, `y`), cell) => PositionCell(position, cell)
      }
      line = cellsInLine.toList.sortBy(_.position.x).map(_.cell.character).mkString(start = "║", end = "║", sep = "")
    } yield line

    if ( lines.isEmpty )
      "<empty>"
    else
      (List(topLine) ++ lines ++ List(bottomLine)).mkString("\n")
  }
}

object State {
  def buildFrom(str: String): State = {
    val lines = str.split("\n").toList
    val width = lines.map(_.length).min
    val height = lines.size
    val cells = for {
      x <- 1 to width
      y <- 1 to height
      Cell(cell) <- Option(lines(y - 1)(x - 1).toString)
    } yield Position(x, y) -> cell
    State(cells.toMap)
  }
}

object ConwayApp extends App {

  val state = State.buildFrom(
    List(
      "---***--",
      "---*-*--",
      "-**-*---"
    ).mkString("\n")
  )

  Iterator.iterate(state)(_.next).take(6).map(_.render).foreach(println)

}
