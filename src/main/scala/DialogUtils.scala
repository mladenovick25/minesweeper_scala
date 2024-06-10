import scala.swing._
import scala.swing.event.ButtonClicked

object DialogUtils {
  def showNumberInputDialog(num: Int):  Option[List[Int]] = {

    // OK and Cancel buttons
    val okButton = new Button("OK")
    val cancelButton = new Button("Cancel")

    // Text fields for number input
    val firstNumberField = new TextField {
      columns = 5
    }
    val secondNumberField = new TextField {
      columns = 5
    }
    val thirdNumberField = new TextField {
      columns = 5
    }
    val forthNumberField = new TextField {
      columns = 5
    }


    val dialog = new Dialog {
      title = "Input Required"

      contents = new BoxPanel(Orientation.Vertical) {
        contents += new Label("Please enter the two numbers:")
        contents += new GridPanel(2, 2) {
          if(num == 2){
            contents += new Label("Y coordinate:")
            contents += firstNumberField
            contents += new Label("X coordinate:")
            contents += secondNumberField
          }
          if(num == 4){
            contents += new Label("Y coordinate top left:")
            contents += firstNumberField
            contents += new Label("X coordinate top left:")
            contents += secondNumberField
            contents += new Label("Y coordinate bottom right:")
            contents += thirdNumberField
            contents += new Label("X coordinate bottom right:")
            contents += forthNumberField
          }
        }
        contents += new FlowPanel {
          contents += okButton
          contents += cancelButton
        }
      }

      // Dialog settings
      modal = true
      size = new Dimension(300, 150)
    }

    var result: Option[List[Int]] = None

    // Action listeners for buttons
    dialog.listenTo(okButton, cancelButton)
    dialog.reactions += {
      case ButtonClicked(`okButton`) =>
        try {
          val firstNum = firstNumberField.text.toInt
          val secondNum = secondNumberField.text.toInt
          if(num == 4){
            val thirdNum = thirdNumberField.text.toInt
            val forthNum = forthNumberField.text.toInt
            result = Some(List(firstNum, secondNum, thirdNum, forthNum))
          }
          else
            result = Some(List(firstNum, secondNum))
          dialog.close()
        } catch {
          case _: NumberFormatException =>
            Dialog.showMessage(
              message = "Invalid input. Please enter valid numbers.",
              title = "Error",
              messageType = Dialog.Message.Error
            )
        }
      case ButtonClicked(`cancelButton`) =>
        dialog.close()
    }

    // Center the dialog on the screen
    val screenSize = java.awt.Toolkit.getDefaultToolkit.getScreenSize
    val x = (screenSize.width - dialog.size.width) / 2
    val y = (screenSize.height - dialog.size.height) / 2
    dialog.location = new Point(x, y)

    dialog.open()
    result
  }
}