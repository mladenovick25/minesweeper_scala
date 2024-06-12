import scala.collection.mutable.ArrayBuffer

class IsometryMaker (oldOp: Operations, updatePanel: () => Unit, resetGame: () => Unit) {

  def dummyFunction() = {}

  var MYGAME = new Minesweeper(oldOp.game.width, oldOp.game.height, 0)
  var newOP = new Operations(MYGAME, dummyFunction, dummyFunction, ArrayBuffer[(String, (MyRectangle, ArrayBuffer[Int]) => MyRectangle)]())


  def createNewIsometry(expressionString: String): (MyRectangle, ArrayBuffer[Int]) => MyRectangle = {
    val ec = new ExpressionController(oldOp.myGlobalArrayFunctions)
    ec.loadExpression(expressionString)
    var nextISOS = ec.makeItReal()


    //val MYGAMErect = new MyRectangle(game.grid, 0, 0, game.height, game.width)
    MYGAME = new Minesweeper(oldOp.game.width, oldOp.game.height, 0)
    MYGAME.copyToMe(oldOp.game)

    newOP = new Operations(MYGAME, dummyFunction, dummyFunction, ArrayBuffer[(String, (MyRectangle, ArrayBuffer[Int]) => MyRectangle)]())

    val newestIsometry = composition(ec, nextISOS)

    oldOp.myGlobalArrayFunctions += ((ec.isometryName, newestIsometry))

    // UPIS U FAJL

    newestIsometry
  }


  def useIsometry(rect: MyRectangle, nextIsos: ArrayBuffer[(Either[(MyRectangle, ArrayBuffer[Int]) => MyRectangle, Isometry], Isometry, ArrayBuffer[Int])], ab: ArrayBuffer[Int]): MyRectangle = {


    val isoTuple = nextIsos.lastOption

    isoTuple match {
      case Some(iso) =>
        println(s"Popped element: $iso")
        nextIsos.trimEnd(1)
        // operacija /////////////////////////////////


        iso._1 match {
          case Left(func) =>
            var newArguments = ArrayBuffer[Int]()
            for (elem <- iso._3) {
              if (elem < -300) {
                var pomel = 0 - (elem + 300) - 1
                pomel = ab(pomel)
                newArguments += pomel
              }
              else
                newArguments += elem
            }
            println("USO OVDE AAAAAAAAAAAAAAAA " + iso._3)
            rect.printRect()
            val nextRect = func(rect, newArguments)
            nextRect.printRect()
            println("IZASO OVDE AAAAAAAAAAAAAAAA")
            nextRect.resetChangeNums()
            useIsometry(nextRect, nextIsos, ab)
          case Right(isoo) =>

            // ######################################################

            var yCoor = iso._3(0)
            if (yCoor < -300) {
              yCoor = 0 - (yCoor + 300) - 1
              yCoor = ab(yCoor)
            }
            var xCoor = iso._3(1)
            if (xCoor < -300) {
              xCoor = 0 - (xCoor + 300) - 1
              xCoor = ab(xCoor)
            }

            // ######################################################

            val nextRect = isoo.extendMe(iso._2, newOP, yCoor + newOP.CHANGEDY, xCoor + newOP.CHANGEDX, rect, true, false)

            if (!nextRect.isCorrect || !rect.isCorrect)
              return new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

            rect.printRect()
            nextRect.addToGame(newOP, iso._2.isTransparent())
            //rect.clearOutOfImage(newOP, nextRect)

            // NEW COORDINATES
            newOP.CHANGEDY += nextRect.rowChange
            newOP.CHANGEDX += nextRect.colChange
            nextRect.resetChangeNums()

            useIsometry(nextRect, nextIsos, ab)
          /////////////////////////////////////////////
        }

      /////////////////////////////////////////////

      case None =>
        println("No element was popped because the array was empty")
        MinesweeperGUI.game = newOP.getGame()
        oldOp.game = newOP.getGame()
        updatePanel()
        resetGame()
        rect
    }
  }

  //val rect = new MyRectangle(game.grid, 3, 1, 3, 3)

  def composition(ec: ExpressionController, nextIsoS: ArrayBuffer[(Either[(MyRectangle, ArrayBuffer[Int]) => MyRectangle, Isometry], Isometry, ArrayBuffer[Int])]): (MyRectangle, ArrayBuffer[Int]) => MyRectangle = {
    (x: MyRectangle, ab: ArrayBuffer[Int]) => {
      useIsometry(x, nextIsoS, ab)
    }
  }


}
