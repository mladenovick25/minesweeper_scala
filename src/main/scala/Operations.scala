import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, CheckBox, FlowPanel, Orientation, TextField}
import DialogUtils._

import scala.collection.mutable.ArrayBuffer


class Operations (gamee:Minesweeper, updatePanel: () => Unit, resetGame: () => Unit, globalArrayFunctions : ArrayBuffer[(String, (MyRectangle, ArrayBuffer[Int]) => MyRectangle)]) extends BoxPanel(Orientation.Vertical)  {

  var game = gamee

  val myGlobalArrayFunctions = globalArrayFunctions

  var CHANGEDY = 0
  var CHANGEDX = 0

  val addFirstRowButton = new Button("+ first row")
  val addLastRowButton = new Button("+ last row")
  val deleteFirstRowButton = new Button("- first row")
  val deleteLastRowButton = new Button("- last row")

  val addFirstColumnButton = new Button("+ first column")
  val addLastColumnButton = new Button("+ last column")
  val deleteFirstColumnButton = new Button("- first column")
  val deleteLastColumnButton = new Button("- last column")


  val toggleCell = new Button("Toogle Cell")
  val clearRectangle = new Button("Clear Rectangle")
  val option1Checkbox = new CheckBox("Transparent")
  val option2Checkbox = new CheckBox("Extendable")
  val rotationButton = new Button("Rotate Cell")


  val expressionField = new TextField {
    columns = 50
  }
  val functionCallerField = new TextField {
    columns = 50
  }



  contents += new FlowPanel {
    contents += addFirstRowButton
    contents += addLastRowButton
    contents += deleteFirstRowButton
    contents += deleteLastRowButton
  }

  contents += new FlowPanel {
    contents += addFirstColumnButton
    contents += addLastColumnButton
    contents += deleteFirstColumnButton
    contents += deleteLastColumnButton
  }

  contents += new FlowPanel {
    contents += toggleCell
    contents += clearRectangle
    // Add two new checkboxes to the panel
    contents += option1Checkbox
    contents += option2Checkbox
    contents += rotationButton
  }

  contents += expressionField
  contents += functionCallerField

  listenTo(addFirstRowButton, addLastRowButton, deleteFirstRowButton, deleteLastRowButton,
    addFirstColumnButton, addLastColumnButton, deleteFirstColumnButton, deleteLastColumnButton,
    toggleCell, clearRectangle, rotationButton)


  reactions += {
    case ButtonClicked(`addFirstRowButton`) => addFirstRow()
    case ButtonClicked(`addLastRowButton`) => addLastRow()
    case ButtonClicked(`deleteFirstRowButton`) => deleteFirstRow()
    case ButtonClicked(`deleteLastRowButton`) => deleteLastRow()
    case ButtonClicked(`addFirstColumnButton`) => addFirstColumn()
    case ButtonClicked(`addLastColumnButton`) => addLastColumn()
    case ButtonClicked(`deleteFirstColumnButton`) => deleteFirstColumn()
    case ButtonClicked(`deleteLastColumnButton`) => deleteLastColumn()
    case ButtonClicked(`toggleCell`) =>
      val nums = showNumberInputDialog(2)
      nums match {
        case Some(numbers) if numbers.length == 2 =>
          val first = numbers.head
          val second = numbers(1)
          if (game.height > first && game.width > second) {
            game.grid(first)(second).isMine = !game.grid(first)(second).isMine
            if(game.grid(first)(second).isMine)  game.numMines += 1
            else {
              if(game.numMines > 1) {
                game.numMines -= 1
                resetGame()
              }
              else{
                //vrati ako ne sme nize
                game.grid(first)(second).isMine = !game.grid(first)(second).isMine
              }
            }
          } else {
            println("Out of range")
          }
        case None =>
          println("None inserted")
      }
    case ButtonClicked(`clearRectangle`) =>
      val nums = showNumberInputDialog(4)
      nums match {
        case Some(numbers) if numbers.length == 4 =>
          val first = numbers.head
          val second = numbers(1)
          val third = numbers(2)
          val forth = numbers(3)
          if (game.height > first && game.width > second && game.height > third && game.width > forth) {
            val numMinesHelp = game.numMines
            for (y <- first until third + 1) {
              for (x <- second until forth + 1) {
                if(game.grid(y)(x).isMine) {
                  game.numMines -= 1
                  //game.grid(y)(x).isMine = false
                }
              }
            }
            if(game.numMines > 0){
              for (y <- first until third + 1) {
                for (x <- second until forth + 1) {
                  if (game.grid(y)(x).isMine) {
                    game.grid(y)(x).isMine = false
                  }
                }
              }
              resetGame()
            }
            else
              game.numMines = numMinesHelp
          } else {
            println("Out of range")
          }
        case None =>
          println("None inserted")
      }
    case ButtonClicked(`rotationButton`) =>

      //contents += expressionField
      //contents += functionCallerField

      val isoMaker = new IsometryMaker(this, updatePanel, resetGame)

      val rect = new MyRectangle(game.grid, 3, 1, 3, 3)

      val newFunct = isoMaker.createNewIsometry(expressionField.text)


      //val newInputStr = "Oper(x,y) = LDiag(x,y) <- RDiag(x,y)"

      val centralna = ArrayBuffer[Int]((game.height / 2).toInt, (game.width / 2).toInt)
      newFunct(rect, centralna)




  }

  def addFirstRow(): Unit = {
    if (game.height == 11 || (game.height == 16 && game.width == 16)) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }
    if(game.gameOver)
      resetGame()
    game.height += 1
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    for (x <- 0 until game.width) {
      newGrid(0)(x) = new Cell()
    }

    // Copy existing elements to new grid
    for (y <- 0 until game.height - 1) {
      for (x <- 0 until game.width) {
        newGrid(y + 1)(x) = game.grid(y)(x)
      }
    }
    game.grid = newGrid
    updatePanel()
  }

  def addLastRow(): Unit = {
    if (game.height == 11 || (game.height == 16 && game.width == 16)) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }
    if (game.gameOver)
      resetGame()

    game.height += 1
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    // Copy existing elements to new grid
    for (y <- 0 until game.height - 1) {
      for (x <- 0 until game.width) {
        newGrid(y)(x) = game.grid(y)(x)
      }
    }

    // Initialize the new row with default Cell values
    for (x <- 0 until game.width) {
      newGrid(game.height - 1)(x) = new Cell()
    }

    // Update grid reference to the new grid
    game.grid = newGrid
    updatePanel()
  }

  def deleteFirstRow(): Unit = {
    var counter_mines_first = 0

    for (x <- 0 until game.width) {
      if(game.grid(0)(x).isMine)
        counter_mines_first += 1
    }

    if(game.check_difficulty(game.numMines - counter_mines_first) == 0) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }

    if (game.gameOver)
      resetGame()

    game.height -= 1
    game.numMines -= counter_mines_first
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    // Copy existing elements to new grid
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width) {
        newGrid(y)(x) = game.grid(y + 1)(x)
      }
    }
    game.grid = newGrid
    updatePanel()
  }

  def deleteLastRow(): Unit = {

    var counter_mines_first = 0

    for (x <- 0 until game.width) {
      if (game.grid(game.height - 1)(x).isMine)
        counter_mines_first += 1
    }

    if (game.check_difficulty(game.numMines - counter_mines_first) == 0) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }

    if (game.gameOver)
      resetGame()

    game.height -= 1
    game.numMines -= counter_mines_first
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    // Copy existing elements to new grid
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width) {
        newGrid(y)(x) = game.grid(y)(x)
      }
    }
    game.grid = newGrid
    updatePanel()
  }









  def addFirstColumn(): Unit = {
    if (game.width == 11 || (game.height == 16 && game.width == 16)) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }
    if (game.gameOver)
      resetGame()
    game.width += 1
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    for (y <- 0 until game.height) {
      newGrid(y)(0) = new Cell()
    }

    // Copy existing elements to new grid
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width - 1) {
        newGrid(y)(x + 1) = game.grid(y)(x)
      }
    }
    game.grid = newGrid
    updatePanel()
  }

  def addLastColumn(): Unit = {
    if (game.width == 11 || (game.height == 16 && game.width == 16)) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }
    if (game.gameOver)
      resetGame()

    game.width += 1
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    // Copy existing elements to new grid
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width - 1) {
        newGrid(y)(x) = game.grid(y)(x)
      }
    }

    // Initialize the new row with default Cell values
    for (y <- 0 until game.height) {
      newGrid(y)(game.width - 1) = new Cell()
    }

    // Update grid reference to the new grid
    game.grid = newGrid
    updatePanel()
  }

  def deleteFirstColumn(): Unit = {
    var counter_mines_first = 0

    for (y <- 0 until game.height) {
      if (game.grid(y)(0).isMine)
        counter_mines_first += 1
    }

    if (game.check_difficulty(game.numMines - counter_mines_first) == 0) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }

    if (game.gameOver)
      resetGame()

    game.width -= 1
    game.numMines -= counter_mines_first
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    // Copy existing elements to new grid
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width) {
        newGrid(y)(x) = game.grid(y)(x + 1)
      }
    }
    game.grid = newGrid
    updatePanel()
  }

  def deleteLastColumn(): Unit = {
    var counter_mines_first = 0

    for (y <- 0 until game.height) {
      if (game.grid(y)(game.height - 1).isMine)
        counter_mines_first += 1
    }

    if (game.check_difficulty(game.numMines - counter_mines_first) == 0) {
      //Dialog.showMessage(s"Cannot add more rows. Maximum rows are riched for this difficulty.", title = "Sorry!")
      return
    }

    if (game.gameOver)
      resetGame()

    game.width -= 1
    game.numMines -= counter_mines_first
    val newGrid = Array.ofDim[Cell](game.height, game.width)

    // Copy existing elements to new grid
    for (y <- 0 until game.height) {
      for (x <- 0 until game.width) {
        newGrid(y)(x) = game.grid(y)(x)
      }
    }
    game.grid = newGrid
    updatePanel()
  }

  def getGame(): Minesweeper = {
    return game
  }

}
