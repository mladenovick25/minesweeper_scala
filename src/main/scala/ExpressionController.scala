import scala.collection.mutable.ArrayBuffer

class ExpressionController(nameSpace : ArrayBuffer[(String, (MyRectangle, ArrayBuffer[Int]) => MyRectangle)]) {

  var expression = ""

  var isometryName = ""
  var arguments = ArrayBuffer[(String, Int)]()

  var bodyOperations = ArrayBuffer[(String, Boolean, Boolean, ArrayBuffer[Int])]()

  def loadExpression(exp : String)={
    expression = exp

    val splitRegex = """\s*=\s*|\s*<-\s*"""
    val parts = exp.split(splitRegex)

    // Step 2: Define a regex to match function names and arguments
    val funcPattern = """(\w+)\(([^)]*)\)""".r

    // Step 3: Extract functions and their arguments
    val extractedFunctions = parts.flatMap { part =>
      funcPattern.findFirstMatchIn(part).map { m =>
        val funcName = m.group(1)
        val args = m.group(2).split(",").map(_.trim).toList
        (funcName, args)
      }
    }
//      row <- y until y + height
    var ind = 0
    for {
      ind <- extractedFunctions.indices
    } {
      if(ind == 0)
        firstControlled(extractedFunctions(ind))
      else
        othersControlled(extractedFunctions(ind))
    }

    //bodyOperations.remove(1)
    println(bodyOperations)
  }

  def firstControlled(newIsometry : (String, List[String])): Unit = {
    isometryName = newIsometry._1
    var pom : Int = -301
    newIsometry._2.foreach(elem =>{
      arguments += ((elem, pom))
      pom -= 1
    })
    println(arguments)
  }

  def othersControlled(newIsometry: (String, List[String])): Unit = {
    var nameIso = newIsometry._1
    var pom_arr = new ArrayBuffer[Int]()

    newIsometry._2.foreach(elem => {

      val numberOption = elem.toIntOption

      numberOption match {
        case Some(number) =>
          pom_arr += number
        case None =>
          for(ela <- arguments){
            if(ela._1 == elem)
              pom_arr += ela._2
          }
      }


    })

    var e = false
    var t = false

    if(nameIso(nameIso.length - 1) == 'E') {
      e = true
      nameIso = nameIso.substring(0, nameIso.length - 1)
    }
    if (nameIso(nameIso.length - 1) == 'T') {
      t = true
      nameIso = nameIso.substring(0, nameIso.length - 1)
    }

    bodyOperations += ((nameIso, t, e, pom_arr))
  }

  def makeItReal(): ArrayBuffer[(Either[(MyRectangle,ArrayBuffer[Int]) => MyRectangle, Isometry], Isometry, ArrayBuffer[Int])] = {
    //deo IZOMETRIJA
    val retArrayBuff = ArrayBuffer[(Either[(MyRectangle,ArrayBuffer[Int]) => MyRectangle, Isometry], Isometry, ArrayBuffer[Int])]()
    var rotic = new Isometry {}
    var levi = new Isometry {}

    for (elem <- bodyOperations) {
      (elem._2, elem._3) match {
        case (true, true) =>
          rotic = new TransparentOp with ExtendableOp {}
        case (true, false) =>
          rotic = new TransparentOp with InextendibleOp {}
        case (false, true) =>
          rotic = new NonTransparentOp with ExtendableOp {}
        case (false, false) =>
          rotic = new NonTransparentOp with InextendibleOp {}
      }

      var insertedAlready = false
      elem._1 match {
        case "RRot" =>
          levi = new RightRotation {}
        case "LRot" =>
          levi = new LeftRotation {}
        case "Hor" =>
          levi = new HorizontalSymetry {}
        case "Ver" =>
          levi = new VerticalSymetry {}
        case "RDiag" =>
          levi = new RightDiagonalSymetry {}
        case "LDiag" =>
          levi = new LeftDiagonalSymetry {}
        case _ =>
          insertedAlready = true
          for (nameF <- nameSpace) {
            if(elem._1 == nameF._1){
              println("BAR SAM GA UBACIOOOO")
              retArrayBuff += ((Left(nameF._2), rotic, elem._4))
            }
          }
      }
      if(!insertedAlready)
        retArrayBuff += ((Right(levi), rotic, elem._4))
    }

    retArrayBuff
  }

}


object JDoodle {

  def main(args: Array[String]) {
    val inputStr = "Oper(x,y,z) = RotTE(x,3) <- LdijT(y,x) <- HorE(1,z)"

    var arFunkcije = ArrayBuffer[(String, (MyRectangle, ArrayBuffer[Int]) => MyRectangle)]()

    val ec = new ExpressionController(arFunkcije)
    ec.loadExpression(inputStr)
    println(ec.arguments)
    println(ec.bodyOperations)
  }
}

