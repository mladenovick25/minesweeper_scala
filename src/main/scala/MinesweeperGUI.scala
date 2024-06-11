import MinesweeperGUI.game

import scala.swing._
import scala.swing.event._
import java.awt.event.{MouseAdapter, MouseEvent}
import scala.util.Random
import javax.swing.{Timer => SwingTimer}
import java.awt.Font
import java.awt.Dimension
import scala.annotation.tailrec
import java.io.File
import scala.io.Source
import java.io.{File, PrintWriter}

object MinesweeperGUI extends SimpleSwingApplication {
  var game = new Minesweeper(9, 9, 10)
  private var buttons = Array.ofDim[Button](game.height, game.width)

  def top: MainFrame = new MainFrame {
    title = "Minesweeper"

    var elapsedSeconds = 0
    val timerLabel = new Label("000")
    var scoreCounter = 0
    var flagCounter = game.maxMines()
    val flagsLabel = new Label(f"$flagCounter%03d")
    val smileyButton = new Button("☺")

    val timer = new SwingTimer(1000, Swing.ActionListener(_ => {
      elapsedSeconds += 1
      timerLabel.text = f"$elapsedSeconds%03d"
    }))

    def resetGame(): Unit = {
      timer.stop()
      elapsedSeconds = 0
      timerLabel.text = "000"
      scoreCounter = 0
      flagCounter = game.maxMines()
      flagsLabel.text = f"$flagCounter%03d"
      resetTheButtons()
    }

    def resetTheButtons(): Unit = {
      game.openCells = 0
      game.gameOver = false
      for (y <- 0 until game.height; x <- 0 until game.width) {
        game.grid(y)(x).revealed = false
        game.grid(y)(x).flag = false
        buttons(y)(x).text = ""
        buttons(y)(x).background = null
      }
    }

    def createGamePanel(): GridPanel = {
      new GridPanel(game.height, game.width) {
        //if(!game.gameOver)
        game.openCells = 0
        buttons = Array.ofDim[Button](game.height, game.width)

        // ovde se pamte zastave i vreme
        flagCounter = game.maxMines()
        flagsLabel.text = f"$flagCounter%03d"

        for (y <- 0 until game.height; x <- 0 until game.width) {
          val button = new Button {
            preferredSize = new Dimension(50, 50) // Set fixed width and height
            reactions += {
              case ButtonClicked(_) =>
                if (!game.grid(y)(x).revealed && !game.grid(y)(x).flag) {
                  game.grid(y)(x).revealed = true
                  if (game.grid(y)(x).isMine) {
                    text = "#"
                    background = java.awt.Color.RED
                    timer.stop()
                    clickedMineOpenAll()
                  } else {
                    val mineCount = game.countMines(y, x)
                    if (mineCount == 0) {
                      game.grid(y)(x).revealed = false
                      revealConnectedZeros(List((y, x)))
                    }
                    text = mineCount.toString
                    background = java.awt.Color.LIGHT_GRAY
                    if (mineCount != 0) {
                      if(game.checkForVictory(false))
                        showWinMessage()
                    }
                    if (!timer.isRunning && !game.gameOver)
                      timer.start()
                    scoreCounter += 1
                  }
                }
            }
          }

          if (game.grid(y)(x).flag){
            button.text = "F"
            button.background = java.awt.Color.YELLOW
            flagCounter -= 1
            flagsLabel.text = f"$flagCounter%03d"
          }

          if (game.grid(y)(x).revealed) {
            if (!game.grid(y)(x).isMine) {
              val mineCount = game.countMines(y, x)
              button.text = mineCount.toString
              button.background = java.awt.Color.LIGHT_GRAY
              if (game.checkForVictory(false))
                showWinMessage()
            }
            else{
              button.text = "#"
              button.background = java.awt.Color.RED
              game.checkForVictory(true)
            }
          }

          button.peer.addMouseListener(new MouseAdapter {
            override def mousePressed(e: MouseEvent): Unit = {
              if (e.getButton == MouseEvent.BUTTON3) {
                if(game.gameOver)
                  return

                if (!timer.isRunning && !game.gameOver)
                  timer.start()

                if (!game.grid(y)(x).revealed) {
                  game.grid(y)(x).flag = !game.grid(y)(x).flag
                  if (game.grid(y)(x).flag) {
                    if (flagCounter > 0) {
                      button.text = "F"
                      button.background = java.awt.Color.YELLOW
                      flagCounter -= 1
                      flagsLabel.text = f"$flagCounter%03d"
                    } else
                      game.grid(y)(x).flag = !game.grid(y)(x).flag
                  } else {
                    button.text = ""
                    button.background = null
                    flagCounter += 1
                    flagsLabel.text = f"$flagCounter%03d"
                  }
                }
              }
            }
          })

          buttons(y)(x) = button
          contents += button
        }


        @tailrec
        private def revealConnectedZeros(queue: List[(Int, Int)]): Unit = queue match {
          case Nil => // If the queue is empty, do nothing
          case (y, x) :: tail => // Decompose the head and tail of the queue
            game.grid(y)(x).revealed match {
              case true => // If the current cell is already revealed, continue with the rest of the queue
                revealConnectedZeros(tail)
              case false => // If the current cell is not revealed
                game.grid(y)(x).revealed = true
                buttons(y)(x).text = game.countMines(y, x).toString
                buttons(y)(x).background = java.awt.Color.LIGHT_GRAY
                if (game.checkForVictory(false))
                  showWinMessage()

                if (game.countMines(y, x) == 0) { // If the cell has zero mines around it
                  val neighbors = for {
                    dy <- -1 to 1
                    dx <- -1 to 1
                    ny = y + dy
                    nx = x + dx
                    if ny >= 0 && ny < game.height && nx >= 0 && nx < game.width && !game.grid(ny)(nx).revealed
                  } yield (ny, nx)
                  revealConnectedZeros(tail ++ neighbors) // Add neighbors to the queue and recurse
                } else {
                  revealConnectedZeros(tail) // Recurse with the remaining queue
                }
            }
        }
      }
    }

    def clickedMineOpenAll(): Unit = {
      game.openCells = 0
      for (y <- 0 until game.height; x <- 0 until game.width) {
        game.grid(y)(x).revealed = true
        game.grid(y)(x).flag = false
        buttons(y)(x).text = if (game.grid(y)(x).isMine) "#" else game.countMines(y, x).toString
        buttons(y)(x).background = if (game.grid(y)(x).isMine) java.awt.Color.RED else java.awt.Color.LIGHT_GRAY
      }
      timer.stop()
      game.gameOver = true
      Dialog.showMessage(
        message = "You lost!",
        title = "Better luck next time",
        messageType = Dialog.Message.Info
      )
    }

    def showWinMessage(): Unit = {
      timer.stop()
      println(game.openCells)
      game.gameOver = true
      Dialog.showMessage(
        message = "You won!\nYour score is: " + (elapsedSeconds + scoreCounter).toString,
        title = "Congratulations",
        messageType = Dialog.Message.Info
      )
    }


    val newGameButton = new Button("New Game")
    val hintButton = new Button("Hint")
    val importButton = new Button("Import")
    val exportButton = new Button("Export")
    val playSeqButton = new Button("Play Sequence")
    val quitButton = new Button("Quit")


    val buttonsPanel = new BoxPanel(Orientation.Vertical) {
      val topButtonsPanel = new FlowPanel() {
        contents += newGameButton
        contents += hintButton
        contents += importButton
        contents += exportButton
        contents += playSeqButton
        contents += quitButton
        //preferredSize = new Dimension(game.width * 50, 50)
      }

      val addRowsColumnsPanel = new Operations(game, updateGamePanel, resetGame)


      val listView1 = new ListView(Seq("Experts:", "Item 1", "Item 2", "Item 3"))
      val listView2 = new ListView(Seq("Intermediates:", "Item A", "Item B", "Item C"))
      val listView3 = new ListView(Seq("Beginners:", "Entry X", "Entry Y", "Entry Z"))

      listView1.preferredSize = new Dimension(100, listView1.font.getSize * 8)
      listView2.preferredSize = new Dimension(100, listView2.font.getSize * 8)
      listView3.preferredSize = new Dimension(100, listView3.font.getSize * 8)

      val listViewsPanel = new BoxPanel(Orientation.Horizontal) {
        contents += listView1
        contents += Swing.HGlue
        contents += listView2
        contents += Swing.HGlue
        contents += listView3
      }

      contents += topButtonsPanel
      contents += addRowsColumnsPanel
      contents += listViewsPanel
    }

    val statusPanel = new BoxPanel(Orientation.Horizontal) {
      contents += flagsLabel
      contents += Swing.HGlue
      contents += smileyButton
      contents += Swing.HGlue
      contents += timerLabel
      border = Swing.EmptyBorder(10, 10, 10, 10)
      //preferredSize = new Dimension(game.width * 50, 50)
    }

    var gamePanel = createGamePanel()

    def updateGamePanel(): Unit = {
      contents = new BorderPanel {
        layout(buttonsPanel) = BorderPanel.Position.South
        layout(statusPanel) = BorderPanel.Position.North
        gamePanel = createGamePanel()
        layout(gamePanel) = BorderPanel.Position.Center
      }
      peer.revalidate()
      peer.repaint()
    }

    contents = new BorderPanel {
      layout(buttonsPanel) = BorderPanel.Position.South
      layout(statusPanel) = BorderPanel.Position.North
      layout(gamePanel) = BorderPanel.Position.Center
    }

    //size = new Dimension(game.width * 50, game.height * 50 + 50)

    // listeners for menu
    listenTo(newGameButton, hintButton, importButton, exportButton, playSeqButton, quitButton, smileyButton)
    reactions += {
      case ButtonClicked(`newGameButton`) =>
        val levels = Seq("Beginner", "Intermediate", "Expert")
        val selectedLevel = Dialog.showOptions(
          parent = contents.head,
          message = "Select the game level:",
          title = "New Game",
          optionType = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Question,
          entries = levels,
          initial = 0
        )

        selectedLevel.id match {
          case level @ (0 | 1 | 2) =>
            //println(levels(selectedLevel.id))
            val directory = new File("./src/levels/" + levels(selectedLevel.id).toLowerCase())
            if (directory.exists && directory.isDirectory) {
              val files = directory.listFiles.filter(_.isFile).toSeq

              // Create a dialog with buttons for each file
              val fileDialog = new Dialog {
                title = "Select a level"
                contents = new BoxPanel(Orientation.Vertical) {
                  files.foreach { file =>
                    contents += new Button(file.getName) {
                      reactions += {
                        case ButtonClicked(_) =>
                          val source = scala.io.Source.fromFile(file)
                          val fileContent = try source.mkString finally source.close()
                          println(s"File content of ${file.getName}:\n$fileContent")
                          game.loadLevels(fileContent)
                          updateGamePanel()
                          close()
                      }
                    }
                  }
                  contents += new Button("Random") {
                    reactions += {
                      case ButtonClicked(_) =>
                        game.randomLevel(levels(selectedLevel.id))
                        updateGamePanel()
                        close()
                    }
                  }
                }
                size = new Dimension(300, 200)

                // Center the dialog on the screen
                val screenSize = java.awt.Toolkit.getDefaultToolkit.getScreenSize
                location = new Point((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2)
              }

              fileDialog.open()
            }
          case _ =>
        }

      case ButtonClicked(`hintButton`) =>
        val (yy, xx) = game.hint()
        scoreCounter += 2
        Dialog.showMessage(
          message = "Next move: (" + yy.toString + ", " + xx.toString + ")",
          title = "Hint",
          messageType = Dialog.Message.Info
        )

      case ButtonClicked(`importButton`) =>
        val chooser = new FileChooser(new File("./src/levels/saves"))
        if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
          val selectedFile = chooser.selectedFile
          val source = Source.fromFile(selectedFile)
          val lines = try source.getLines().toSeq finally source.close()

          val lastLine = lines.last
          val Array(maxYStr, maxXStr, _, _, _) = lastLine.split(",")
          val maxY = maxYStr.toInt
          val maxX = maxXStr.toInt

          game.importLevel(lines, maxX + 1, maxY + 1)
          updateGamePanel()
        }

      case ButtonClicked(`exportButton`) =>
        val chooser = new FileChooser(new File("./src/levels"))
        if (chooser.showSaveDialog(null) == FileChooser.Result.Approve) {
          val selectedFile = chooser.selectedFile
          val writer = new PrintWriter(selectedFile)
          try {
            /*for (y <- 0 until game.height) {
              for (x <- 0 until game.width) {
                if(game.grid(y)(x).isMine)
                  writer.print("#")
                else
                  writer.print("-")
              }
              writer.println("")
            }*/
            for (y <- 0 until game.height; x <- 0 until game.width) {
              val cell = game.grid(y)(x)
              writer.println(s"$y,$x,${cell.revealed},${cell.isMine},${cell.flag}")
            }
          } finally {
            writer.close()
          }
          println(s"Game saved to ${selectedFile.getAbsolutePath}")
        }

      case ButtonClicked(`playSeqButton`) =>
        val chooser = new FileChooser(new File("./src/levels/seqs"))
        if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
          val selectedFile = chooser.selectedFile
          val source = Source.fromFile(selectedFile)
          val lines = try source.getLines().toSeq finally source.close()

          if(!game.gameOver) {
            val seqRes = game.playSequence(lines)
            seqRes match {
              case 0 =>
                clickedMineOpenAll()
                peer.revalidate()
                peer.repaint()
              //if (game.checkForVictory(true))
              //showWinMessage()
              case _ =>
                updateGamePanel()
                timer.start()
            }
          }

        }

      case ButtonClicked(`quitButton`) =>
        // Implement quit logic
        sys.exit(0)

      case ButtonClicked(`smileyButton`) =>
        resetGame()
    }

    override def closeOperation(): Unit = {
      timer.stop()
      super.closeOperation()
    }

    // Center the dialog on the screen
    val screenSize = java.awt.Toolkit.getDefaultToolkit.getScreenSize
    location = new Point((screenSize.width - size.width) / 2, (screenSize.height - size.height) / 2)


    // Start the timer
    //timer.start()
  }
}

