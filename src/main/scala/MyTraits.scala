
trait Isometry {
  def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer: Boolean = false): Unit = {
    println("super " + controlMatrix.getGame().grid(rowNew)(colNew).isMine)
  }

  def isExtendible(): Boolean = {
    true
  }

  def isTransparent(): Boolean = {
    true
  }


  def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer: Boolean = false): MyRectangle
}

trait ExtendableOp extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {

    if (rowNew >= controlMatrix.getGame().height) {
      //if (changer) controlMatrix.addLastRow()
      rect.addToRow(rowNew - (controlMatrix.getGame().height - 1), false)
    }
    if (rowNew < 0) {
      //if (changer) controlMatrix.addFirstRow()
      rect.addToRow(0 - rowNew, true)
    }
    if (colNew >= controlMatrix.getGame().width) {
      //if (changer) controlMatrix.addLastColumn()
      rect.addToCol(colNew - (controlMatrix.getGame().width - 1), false)
    }
    if (colNew < 0) {
      //if (changer) controlMatrix.addFirstColumn()
      rect.addToCol(0 - colNew, true)
    }

    //super.operate(controlMatrix, 0 - rowNew, 0 - colNew, rect, tran, changer)

    /*while (rowNew >= controlMatrix.getGame().height){
      if(changer) controlMatrix.addLastRow()
    }
    var rowNewest = rowNew
    while (rowNewest < 0) {
      rowNewest += 1
      if(changer) controlMatrix.addFirstRow()
    }
    if(rowNew < 0)
      rect.addToRow(0 - rowNew, true)
    while (colNew >= controlMatrix.getGame().width) {
      if(changer) controlMatrix.addLastColumn()
    }
    var colNewest = colNew
    while (colNewest < 0) {
      colNewest += 1
      if(changer) controlMatrix.addFirstColumn()
    }
    if (colNew < 0)
      rect.addToCol(0 - colNew)

    println("exten " + rowNew + " " + colNew + " " + tran)
    super.operate(controlMatrix, rowNewest, colNewest, rect, tran, changer)*/
  }

}

trait InextendibleOp extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {

    if (rowNew >= controlMatrix.getGame().height || rowNew < 0 ||
      colNew >= controlMatrix.getGame().width || colNew < 0
    ) {
      println("INEXTE " + rowNew + " " + colNew + " " + tran)
      rect.isCorrect = false
      return
    }

    //super.operate(controlMatrix, rowNew, colNew, rect, tran, changer)
  }

  override def isExtendible(): Boolean = {
    false
  }
}

trait TransparentOp extends Isometry {
  override def isTransparent(): Boolean = {
    true
  }
}

trait NonTransparentOp extends Isometry {
  override def isTransparent() : Boolean = {
    false
  }
}


trait RightRotation extends Isometry {
  /*override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {
    super.operate(controlMatrix, rowNew, colNew, rect, tran, changer)
  }*/

  override def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): MyRectangle = {

    var minrow = rowNew
    var mincol = colNew


    val newRect = new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

    rect.values.foreach { case (row, col, value) =>
      val dy = minrow - row
      val dx = mincol - col

      //controlMatrix.getGame().grid(row)(col).isMine = false

      newRect.insertTuple(minrow - dx, mincol + dy, value)
      mo.operate(controlMatrix, minrow - dx, mincol + dy, rect, value, changer)

      if (mo.isExtendible()) {
        if (0 > minrow - dx) {
          //newRect.addToRow(dx - minrow)
          minrow -= (minrow - dx)
        }
        if (0 > mincol + dy) {
          //newRect.addToCol(0 - dy - mincol)
          mincol -= (mincol + dy)
        }
      }
      rect.addToAnotherRect(newRect)
    }
    newRect
  }
}

trait LeftRotation extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {}

  override def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): MyRectangle = {
    var minrow = rowNew
    var mincol = colNew

    val newRect = new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

    rect.values.foreach { case (row, col, value) =>
      val dy = minrow - row
      val dx = mincol - col

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false
      //controlMatrix.getGame().grid(row)(col).isMine = false

      newRect.insertTuple(minrow + dx, mincol - dy, value)
      mo.operate(controlMatrix, minrow + dx, mincol - dy, rect, value, changer)

      if (mo.isExtendible()) {
        if (0 > minrow + dx)
          minrow -= (minrow + dx)
        if (0 > mincol - dy)
          mincol -= (mincol - dy)
      }
      rect.addToAnotherRect(newRect)
    }
    newRect
  }
}

trait VerticalSymetry extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {}

  override def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): MyRectangle = {
    var mincol = colNew

    val newRect = new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

    rect.values.foreach { case (row, col, value) =>
      //val dy = rowNew - row
      val dx = mincol - col

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      newRect.insertTuple(row, mincol + dx, value)
      mo.operate(controlMatrix, row, mincol + dx, rect, value, changer)

      if(mo.isExtendible()) {
        if (0 > mincol + dx)
          mincol -= (mincol + dx)
      }
      rect.addToAnotherRect(newRect)
    }
    newRect
  }
}

trait HorizontalSymetry extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {}

  override def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran : Boolean = false, changer : Boolean = false): MyRectangle = {
    var minrow = rowNew

    val newRect = new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

    rect.values.foreach { case (row, col, value) =>
      val dy = minrow - row

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      newRect.insertTuple(minrow + dy, col, value)
      mo.operate(controlMatrix, minrow + dy, col, rect, value, changer)

      if(mo.isExtendible()) {
        if (0 > minrow + dy)
          minrow -= (minrow + dy)
      }
      rect.addToAnotherRect(newRect)
    }
    newRect
  }
}

trait RightDiagonalSymetry extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {}
  override def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): MyRectangle = {

    var minrow = rowNew
    var mincol = colNew

    val newRect = new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

    rect.values.foreach { case (row, col, value) =>
      val dy = minrow - row
      val dx = mincol - col

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      /*println((row) + " " + (col))
      println((rowNew) + " " + (colNew))
      println((dy) + " " + (dx))*/

      newRect.insertTuple(minrow + dx, mincol + dy, value)
      mo.operate(controlMatrix, minrow + dx, mincol + dy, rect, value, changer)

      if(mo.isExtendible()){
        if (0 > minrow + dx)
          minrow -= (minrow + dx)
        if (0 > mincol + dy)
          mincol -= (mincol + dy)
      }

      rect.addToAnotherRect(newRect)
    }
    newRect
  }
}

trait LeftDiagonalSymetry extends Isometry {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): Unit = {}
  override def extendMe(mo: Isometry, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false, changer : Boolean = false): MyRectangle = {

    var minrow = rowNew
    var mincol = colNew

    val newRect = new MyRectangle(Array.ofDim[Cell](0, 0), 0, 0, 0, 0)

    rect.values.foreach { case (row, col, value) =>
      val dy = minrow - row
      val dx = mincol - col

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      newRect.insertTuple(minrow - dx, mincol - dy, value)
      mo.operate(controlMatrix, minrow - dx, mincol - dy, rect, value, changer)


      if (mo.isExtendible()) {
        if (0 > minrow - dx)
          minrow -= (minrow - dx)
        if (0 > mincol - dy)
          mincol -= (mincol - dy)
      }
      rect.addToAnotherRect(newRect)
    }
    newRect
  }
}
