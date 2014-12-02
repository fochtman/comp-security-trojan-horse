import scala.swing._
import PhoneBoothUtil._
import scala.swing.BorderPanel.Position._
import event._

object PhoneBoothUI extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "PhoneBooth++"


    val inputNumb = new TextField {
      text = ""
      columns = 5
      horizontalAlignment = Alignment.Center
    }

    val outputNumbs = new TextArea(20, 20) {
      editable = false
    }

    var xs: List[String] = List.empty

    listenTo(inputNumb)
    reactions += {
      case EditDone(inputNumb) =>
        if (inputNumb.text != "911") {
          xs = inputNumb.text :: xs
          outputNumbs.text = PhoneBoothDictionary.translate(inputNumb.text).mkString("\n")
        }
        else {
          outputNumbs.text = loadDictionary(xs.mkString(" ")).mkString(" ")
        }
    }

    lazy val in = new FlowPanel(new Label(" phone number  => "), inputNumb) {
      border = Swing.EmptyBorder(15, 10, 10, 10)
    }

    lazy val out = new ScrollPane(outputNumbs) {
      border = Swing.MatteBorder(15, 10, 10, 10, java.awt.Color.DARK_GRAY)
    }

    contents = new BorderPanel {
      layout(in) = North
      layout(out) = South
    }
  }
}


object PhoneBoothDictionary {
  val dictionary: List[String] = loadDictionary()

  val words = dictionary filter(word => word.forall(chr => chr.isLetter))
  val alphaOmegas = List("ABC", "DEF", "GHI", "JKL", "MNO", "PQRS", "TUV", "WXYZ")
  val digitToLetterGrouping =  (('2' to '9').toList zip alphaOmegas).toMap

  def charCode: Map[Char, Char] =
    for ((digit, str) <- digitToLetterGrouping; ltr <- str) yield ltr -> digit

  def wordCode(word: String): String = word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      (for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest).toSet
    }
  }

  def translate(number: String): Set[String] =
    encode(number) map (_.mkString(" "))
}

