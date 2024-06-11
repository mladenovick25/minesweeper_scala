import scala.util.control.Breaks.break

class MyRectangle(matrix: Array[Array[Cell]], val y: Int, val x: Int,  val height: Int, val width: Int) {

  var values: Array[(Int, Int, Boolean)] = {
    for {
      row <- y until y + height
      col <- x until x + width
    } yield (row, col, matrix(row)(col).isMine)
  }.toArray

  var rowChange = 0
  var colChange = 0
  var rowChangeLast = 0
  var colChangeLast = 0

  var isCorrect = true

  def addToCol(d : Int, first: Boolean): Unit = {
    if(first) colChange += d
    else {
      colChangeLast += d
      return
    }
    for {
      ind <- values.indices
    } {
      values(ind) = (values(ind)._1, values(ind)._2 + d, values(ind)._3)
    }
  }

  def addToRow(d: Int, first: Boolean): Unit = {
    if (first) rowChange += d
    else {
      rowChangeLast += d
      return
    }
    for {
      ind <- values.indices
    } {
      values(ind) = (values(ind)._1 + d, values(ind)._2, values(ind)._3)
    }
  }

  def insertTuple(newTuple: (Int, Int, Boolean)): Unit = {
    values = values :+ newTuple
  }

  def checkRect(): Boolean = {
    if (rowChange > 0 || colChange > 0 || rowChangeLast > 0 || colChangeLast > 0)
      false
    true
  }

  def printRect(): Unit = {
    for {
      ind <- values.indices
    } {
      println(values(ind)._1 + " " + values(ind)._2 + " " + values(ind)._3)
    }
  }

  def addToGame(op : Operations, isTransparent : Boolean): Unit = {
    var ind = 0
    while(ind < rowChange) {
      op.addFirstRow()
      ind += 1
    }
    ind = 0
    while (ind < rowChangeLast) {
      op.addLastRow()
      ind += 1
    }
    ind = 0
    while (ind < colChange) {
      op.addFirstColumn()
      ind += 1
    }
    ind = 0
    while (ind < colChangeLast) {
      op.addLastColumn()
      ind += 1
    }
    for {
      ind <- values.indices
    } {
      if(isTransparent)
        op.getGame().grid(values(ind)._1)(values(ind)._2).isMine = values(ind)._3 || op.getGame().grid(values(ind)._1)(values(ind)._2).isMine
      else
        op.getGame().grid(values(ind)._1)(values(ind)._2).isMine = values(ind)._3
      //(values(ind)._1 + " " + values(ind)._2 + " " + values(ind)._3)
    }

  }

  def resetChangeNums(): Unit = {
    rowChange = 0
    colChange = 0
    rowChangeLast = 0
    colChangeLast = 0
  }

  def addToAnotherRect(rect: MyRectangle): Unit = {
    var ind = rect.rowChange
    if (ind < rowChange) {
      rect.addToRow(rowChange - ind, true)
    }
    ind = rect.rowChangeLast
    if (ind < rowChangeLast) {
      rect.addToRow(rowChangeLast - ind, false)
    }
    ind = rect.colChange
    if (ind < colChange) {
      rect.addToCol(colChange - ind, true)
    }
    ind = rect.colChangeLast
    if (ind < colChangeLast) {
      rect.addToCol(colChangeLast - ind, false)
    }
  }

  def clearOutOfImage(op: Operations, image : MyRectangle): Unit = {
    for {
      ind <- values.indices
    } {
      val y = values(ind)._1
      val x = values(ind)._2

      var throwOut = true

      image.values.foreach(vv =>
          if(vv._1 == y && vv._2 == x){
            throwOut = false
            break
          }
      )
      if(throwOut)
        op.getGame().grid(y)(x).isMine = false
      //(values(ind)._1 + " " + values(ind)._2 + " " + values(ind)._3)
    }
    op.getGame().updateNumMines()
  }


  /*def kompozicija(rect: MyRectangle): MyRectangle => MyRectangle = {

    //  uspesna provera
    if(true) {
      (x: MyRectangle) => {
        rect
      }
    }
    else{
      (x: MyRectangle) => {
        println("Nista")
        new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)
      }
    }
  }

  val duplaRotacija = kompozicija((this))
  duplaRotacija(this)*/

}

