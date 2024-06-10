
trait MatrixOperation {
  def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false): Unit = {
    println("super " +   controlMatrix.getGame().grid(rowNew)(colNew).isMine)
  }
}

trait ExtendableOp extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false): Unit = {

    while (rowNew >= controlMatrix.getGame().height){
      controlMatrix.addLastRow()
    }
    var rowNewest = rowNew
    while (rowNewest < 0) {
      rowNewest += 1
      controlMatrix.addFirstRow()
    }
    while (colNew >= controlMatrix.getGame().width) {
      controlMatrix.addLastColumn()
    }
    var colNewest = colNew
    while (colNewest < 0) {
      colNewest += 1
      controlMatrix.addFirstColumn()
    }

    println("exten " + rowNew + " " + colNew + " " + tran)
    super.operate(controlMatrix, rowNewest, colNewest, rect, tran)
  }
}

trait InextendibleOp extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false): Unit = {

    if (rowNew >= controlMatrix.getGame().height || rowNew < 0 ||
      colNew >= controlMatrix.getGame().width || colNew < 0
    ) {
      return
    }

    super.operate(controlMatrix, rowNew, colNew, rect, tran)
  }
}

trait TransparentOp extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false): Unit = {
    controlMatrix.getGame().grid(rowNew)(colNew).isMine = controlMatrix.getGame().grid(rowNew)(colNew).isMine || tran

    super.operate(controlMatrix, rowNew, colNew, rect, tran)
  }
}

trait NonTransparentOp extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran: Boolean = false): Unit = {
    controlMatrix.getGame().grid(rowNew)(colNew).isMine = tran
    if(tran)
      controlMatrix.getGame().numMines += 1

    println("nontran " + rowNew + " " + colNew + " " + tran)
    super.operate(controlMatrix, rowNew, colNew, rect, tran)
  }
}

trait RightRotation extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran : Boolean = false): Unit = {

    var minrow = 0
    var mincol = 0

    rect.values.foreach { case (row, col, value) =>
      val dy = rowNew - row
      val dx = colNew - col

      controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      super.operate(controlMatrix, rowNew - dx - minrow, colNew + dy - mincol, rect, value)

      if (minrow > rowNew - dx)
        minrow = rowNew - dx
      if (mincol > colNew + dy)
        mincol = colNew + dy
    }
  }
}

trait LeftRotation extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran : Boolean = false): Unit = {

    var minrow = 0
    var mincol = 0

    rect.values.foreach { case (row, col, value) =>
      val dy = rowNew - row
      val dx = colNew - col

      controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      super.operate(controlMatrix, rowNew + dx - minrow, colNew - dy - mincol, rect, value)

      if (minrow > rowNew + dx)
        minrow = rowNew + dx
      if (mincol > colNew - dy)
        mincol = colNew - dy
    }
  }
}

trait VerticalSymetry extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran : Boolean = false): Unit = {

    var mincol = 0

    rect.values.foreach { case (row, col, value) =>
      //val dy = rowNew - row
      val dx = colNew - col

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      super.operate(controlMatrix, row, colNew + dx - mincol, rect, value)

      if (mincol > colNew + dx)
        mincol = colNew + dx
    }
  }
}

trait HorizontalSymetry extends MatrixOperation {
  override def operate(controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran : Boolean = false): Unit = {

    var minrow = 0

    rect.values.foreach { case (row, col, value) =>
      val dy = rowNew - row

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      super.operate(controlMatrix, rowNew + dy - minrow, col, rect, value)

      if (minrow > rowNew + dy)
        minrow = rowNew + dy
    }
  }

  def extendMe(mo: MatrixOperation, controlMatrix: Operations, rowNew: Int, colNew: Int, rect: MyRectangle, tran : Boolean = false): Unit = {
    var minrow = 0

    rect.values.foreach { case (row, col, value) =>
      val dy = rowNew - row

      //controlMatrix.getGame().grid(row - minrow)(col - mincol).isMine = false

      mo.operate(controlMatrix, rowNew + dy - minrow, col, rect, value)

      if (minrow > rowNew + dy)
        minrow = rowNew + dy
    }

  }
}
