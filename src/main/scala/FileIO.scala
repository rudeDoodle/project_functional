import java.io.{PrintWriter, File, FileWriter}
object FileIO {
  //File Input and Output gets handled here
//Write to the file, used for writing energy data
def writeToFile(data:String, path: String): Either[String,Unit] = {
  try {
    val pw = new PrintWriter(new File(path))
    pw.write(data)
    pw.close()
    Right(())
  } catch {
    case e: Exception => Left(s"Error writing file: ${e.getMessage}")
  }
}
//append extra data to file
  def appendToFile(data: String, path: String): Either[String, Unit] = {
    try {
      val pw = new PrintWriter(new FileWriter(new File(path), true))
      pw.write(data)
      pw.close()
      Right(())
    } catch {
      case e: Exception => Left(s"Error appending to file: ${e.getMessage}")
    }
  }
//used for accessing data from the file
  def readFile(path: String): Either[String, String] = {
    val file = new File(path)
    if (!file.exists()) {
      Left(s"File not found: $path")
    } else {
      try {
        val source = scala.io.Source.fromFile(path)
        val content = source.mkString
        source.close()
        Right(content)
      } catch {
        case e: Exception => Left(s"Error reading file: ${e.getMessage}")
      }
    }
  }
}
