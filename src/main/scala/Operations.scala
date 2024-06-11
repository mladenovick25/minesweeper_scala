import scala.swing.event.ButtonClicked
import scala.swing.{BoxPanel, Button, CheckBox, FlowPanel, Orientation}
import DialogUtils._


class Operations (game:Minesweeper, updatePanel: () => Unit, resetGame: () => Unit) extends BoxPanel(Orientation.Vertical)  {

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

      var nextIsoS: Array[(Isometry, Int, Int)] = {
        for {
          ind <- 0 until 4
        } yield (new NonTransparentOp with ExtendableOp with RightRotation {}, 0 , 0)
      }.toArray

      //val MYGAMErect = new MyRectangle(game.grid, 0, 0, game.height, game.width)
      val MYGAME = new Minesweeper(game.width, game.height, 0)
      MYGAME.copyToMe(game)

      def dummyFunction() = {}

      val newOP = new Operations(MYGAME, dummyFunction, dummyFunction)

      def popFromArray(arr: Array[(Isometry, Int, Int)]): (Array[(Isometry, Int, Int)], Option[(Isometry, Int, Int)]) = {
        if (arr.isEmpty) {
          (arr, None) // Return the original array and None if array is empty
        } else {
          (arr.init, Some(arr.last)) // Return the array without the last element and the last element
        }
      }

      def useIsometry(rect: MyRectangle, nextIsos : Array[(Isometry, Int, Int)]) : MyRectangle ={

        val (updatedNextIsos, isoTuple) = popFromArray(nextIsos)

        isoTuple match {
          case Some(iso) =>
            println(s"Popped element: $iso")
            // operacija /////////////////////////////////
            var yCoor = iso._2
            var xCoor = iso._3

            val nextRect = iso._1.extendMe(iso._1, newOP, yCoor + newOP.CHANGEDY, xCoor + newOP.CHANGEDX, rect, true, false)

            if(!nextRect.isCorrect || !rect.isCorrect)
              return new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

            rect.printRect()
            nextRect.addToGame(newOP, iso._1.isTransparent())
            rect.clearOutOfImage(newOP, nextRect)

            // NEW COORDINATES
            newOP.CHANGEDY += nextRect.rowChange
            newOP.CHANGEDX += nextRect.colChange
            nextRect.resetChangeNums()

            useIsometry(nextRect, updatedNextIsos)
            /////////////////////////////////////////////

          case None =>
            println("No element was popped because the array was empty")
            MinesweeperGUI.game = newOP.getGame()
            updatePanel()
            resetGame()
            rect
        }
      }

      val rect = new MyRectangle(game.grid, 3, 1, 3, 3)
      //useIsometry(rect, nextIsoS)

      ///////////////////////////////////

      def kompozicija(nextIsoS : Array[(Isometry, Int, Int)]): MyRectangle => MyRectangle = {
        (x: MyRectangle) => {
          useIsometry(x, nextIsoS)
        }
      }

      val cetvorostrukaRotacija = kompozicija(nextIsoS)
      cetvorostrukaRotacija(rect)

        /////////////////////////////////

      /*val rotationWithExtensions = new RightRotation {}
      //val rotationWithExtensions = new RightDiagonalSymetry {}
      var rotic = new Isometry {}
      //var roticCh = new MatrixOperation {}

      (option1Checkbox.selected, option2Checkbox.selected) match {
        case (true, true) =>
          rotic = new TransparentOp with ExtendableOp {}
          //roticCh = new ExtendableOp {}
        case (true, false) =>
          rotic = new TransparentOp with InextendibleOp {}
          //roticCh = new InextendibleOp {}
        case (false, true) =>
          rotic = new NonTransparentOp with ExtendableOp {}
          // = new ExtendableOp {}
        case (false, false) =>
          rotic = new NonTransparentOp with InextendibleOp {}
          //roticCh = new InextendibleOp {}
      }

      val rectCh = new MyRectangle(game.grid, 3, 1, 3, 3)
      val rect = new MyRectangle(game.grid, 3, 1, 3, 3)


      var rowCENTER = 0
      var colCENTER = 0


      val newRectCh = rotationWithExtensions.extendMe(rotic, this, rowCENTER, colCENTER, rectCh, true, false)

      if(newRectCh.checkRect()){
        //val newRect = rotationWithExtensions.extendMe(rotic, this, 0, 0, rect, true,true)
        newRectCh.addToGame(this)
        //druga
        //val finalRect = rotationWithExtensions.extendMe(rotic, this, newRect.rowChange + 0, newRect.colChange + 0, newRect, true, true)

        println("KITA  " + newRectCh.rowChange + " " + newRectCh.colChange)
        rowCENTER += newRectCh.rowChange
        colCENTER += newRectCh.colChange
        newRectCh.resetChangeNums()
        //newRectCh.printRect()

        val leftDiagonalSym = new LeftDiagonalSymetry {}

        val nextRect = leftDiagonalSym.extendMe(rotic, this, rowCENTER, colCENTER, newRectCh, true, false)
        nextRect.addToGame(this)
        //finalRect.printRect()
      }*/


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
