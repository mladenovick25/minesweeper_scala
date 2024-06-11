import scala.swing._
import scala.swing.event._
import java.awt.event.ActionEvent
import java.io.File
import javax.swing.{Timer => SwingTimer}
import scala.annotation.tailrec
import scala.util.Random
import scala.util.control.Breaks.break



class Minesweeper(val widthArg: Int, val heightArg: Int, numMinesArg: Int) {

  var numMines = numMinesArg
  var width = widthArg
  var height = heightArg
  var grid = Array.ofDim[Cell](height, width)
  var openCells = 0
  var gameOver = false


  initializeGrid()
  placeMines()

  private def initializeGrid(): Unit = {
    for (y <- 0 until height; x <- 0 until width) {
      grid(y)(x) = new Cell()
    }
  }

  private def placeMines(): Unit = {
    @tailrec
    def placeMinesTailRec(minesPlaced: Int): Unit = {
      if (minesPlaced > 0) {
        val x = Random.nextInt(height)
        val y = Random.nextInt(width)

        if (!grid(x)(y).isMine) {
          grid(x)(y).isMine = true
          placeMinesTailRec(minesPlaced - 1)
        }
        else {
          placeMinesTailRec(minesPlaced)
        }
      }
    }

    placeMinesTailRec(numMines)
  }

  def countMines(y: Int, x: Int): Int = {
    (for {
      dy <- -1 to 1
      dx <- -1 to 1
      ny = y + dy
      nx = x + dx
      if ny >= 0 && ny < height && nx >= 0 && nx < width && grid(ny)(nx).isMine
    } yield 1).sum
  }

  def maxMines(): Int = {
    return numMines
  }

  def updateNumMines(): Unit = {
    val yMax = height
    val xMax = width
    numMines = 0

    for {
      y <- 0 to yMax - 1
      x <- 0 to xMax - 1
    } {
      if (grid(y)(x).isMine)
        numMines += 1
    }
  }

  def importLevel(lines: Seq[String], x_max : Int, y_max : Int): Unit = {
    grid = Array.ofDim[Cell](y_max, x_max)
    height = y_max
    width = x_max
    var miness = 0
    for (line <- lines) {
      val Array(y, x, revealed, isMine, flag) = line.split(",")
      grid(y.toInt)(x.toInt) = new Cell()
      grid(y.toInt)(x.toInt).revealed = revealed.toBoolean
      grid(y.toInt)(x.toInt).isMine = isMine.toBoolean
      grid(y.toInt)(x.toInt).flag = flag.toBoolean
      if (grid(y.toInt)(x.toInt).isMine)
        miness += 1
    }
    openCells = 0
    gameOver = false
    numMines = miness
  }

  def loadLevels(filecontent: String): Unit = {
    val lines = filecontent.split("\n").filter(_.nonEmpty)
    val yMax = lines.length
    val xMax = if (yMax > 0) lines.head.length else 0

    grid = Array.ofDim[Cell](yMax, xMax)
    height = yMax
    width = xMax - 1
    var miness = 0

    for ((line, y) <- lines.zipWithIndex) {
      for ((char, x) <- line.zipWithIndex) {
        grid(y)(x) = new Cell()
        grid(y)(x).isMine = char == '#'
        if (grid(y)(x).isMine) {
          miness += 1
        }
      }
    }
    openCells = 0
    gameOver = false
    numMines = miness
  }

  def randomLevel(level: String): Unit = {
    val levels = Seq("Beginner", "Intermediate", "Expert")
    openCells = 0
    gameOver = false
    level match {
      case "Beginner" =>
        height = 9
        width = 9
        numMines = 10
      case "Intermediate" =>
        height = 16
        width = 16
        numMines = 30
      case "Expert" =>
        height = 16
        width = 30
        numMines = 99
    }

    grid = Array.ofDim[Cell](height, width)

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        grid(y)(x) = new Cell()
        grid(y)(x).revealed = false
        grid(y)(x).isMine = false
        grid(y)(x).flag = false
      }
    }

    placeMines()

  }

  def checkForVictory(isMineClicked: Boolean): Boolean = {
    openCells += 1
    //println(openCells)
    if(openCells + numMines == width * height && !isMineClicked)
      true
    else
      false
  }

  def hint(): (Int, Int) = {
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        if(!grid(y)(x).isMine && !grid(y)(x).revealed && !grid(y)(x).flag)
          return (y, x)
      }
    }
    (-1, -1)
  }

  def playSequence(lines: Seq[String]): Int = {
    val pattern = """([LR])\((\d+),(\d+)\)""".r
    lines.foreach {
      case pattern(direction, yy, xx) =>
        //println(s"Direction: $direction, x: $x, y: $y")
        val x = xx.toInt
        val y = yy.toInt
        if(x < width && y < height){
          direction match {
            case "L" =>
              if (!grid(y)(x).isMine && !grid(y)(x).revealed && !grid(y)(x).flag) {
                if(countMines(y, x) == 0){
                  revealConnectedZerosMap(List((y, x)))
                }
                else
                  grid(y)(x).revealed = true
              }
              if (grid(y)(x).isMine && !grid(y)(x).flag) {
                println("ovde")
                return 0
              }
            case "R" =>
              if (!grid(y)(x).revealed)
                grid(y)(x).flag = !grid(y)(x).flag
            case _ =>
              println(s"Unknown direction: $direction")
              return -1
          }
        }
        else {
          println(s"Coordinates out of bounds: ($x, $y)")
          return -1
        }
      case _ =>
        println("No match")
        return -1
    }
    1
  }

  @tailrec
  private def revealConnectedZerosMap(queue: List[(Int, Int)]): Unit = queue match {
    case Nil => // If the queue is empty, do nothing
    case (y, x) :: tail => // Decompose the head and tail of the queue
      grid(y)(x).revealed match {
        case true => // If the current cell is already revealed, continue with the rest of the queue
          revealConnectedZerosMap(tail)
        case false => // If the current cell is not revealed
          grid(y)(x).revealed = true

          if (countMines(y, x) == 0) { // If the cell has zero mines around it
            val neighbors = for {
              dy <- -1 to 1
              dx <- -1 to 1
              ny = y + dy
              nx = x + dx
              if ny >= 0 && ny < height && nx >= 0 && nx < width && !grid(ny)(nx).revealed
            } yield (ny, nx)
            revealConnectedZerosMap(tail ++ neighbors) // Add neighbors to the queue and recurse
          } else {
            revealConnectedZerosMap(tail) // Recurse with the remaining queue
          }
      }
  }

  def check_difficulty(mines: Int): Int = {
    if(height <= 10 && width <= 10) {
      if(mines <= 10 && mines > 0)
        return 1
      else
        return 0
    } else if(height <= 16 && width <= 16) {
      if (mines <= 50 && mines > 10)
        return 2
      else
        return 0
    } else {
      if (mines > 50)
        return 1
      else
        return 0
    }
  }


  def copyToMe(minesweeper: Minesweeper): Unit = {
    val yMax = minesweeper.height
    val xMax = minesweeper.width
    numMines = 0

    for {
      y <- 0 to yMax - 1
      x <- 0 to xMax - 1
    } {
      grid(y)(x).isMine = minesweeper.grid(y)(x).isMine
      if(grid(y)(x).isMine)
        numMines += 1
    }
  }

}