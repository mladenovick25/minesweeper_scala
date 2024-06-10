class MyRectangle(matrix: Array[Array[Cell]], val y: Int, val x: Int,  val height: Int, val width: Int) {

  val values: Array[(Int, Int, Boolean)] = {
    for {
      row <- y until y + height
      col <- x until x + width
    } yield (row, col, matrix(row)(col).isMine)
  }.toArray



}

