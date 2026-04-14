object Main extends App {
  val dataDir = "data"
  val dir = new java.io.File(dataDir)
  if (!dir.exists()) dir.mkdirs()

  def printMenu(): Unit = {
    println("\nRenewable Energy Plant System")
    println("1. Get Solar Data")
    println("2. Get Wind Data")
    println("3. Get Hydro Data")
    println("4. View data")
    println("5. Exit")
  }

  def mainLoop(): Unit = {
    printMenu()
    print("Choose option: ")
    val choice = scala.io.StdIn.readLine()

    choice match {
      case "5" => println("Goodbye.")
      case _ =>
        handleChoice(choice)
        mainLoop()
    }
  }

  def handleChoice(choice: String): Unit = choice match {
    case "1" => getAndSave("Solar", GetData.fetchSolar)
    case "2" => getAndSave("Wind", GetData.fetchWind)
    case "3" => getAndSave("Hydro", GetData.fetchHydro)
    case "4" =>
      print("Enter filename (solar.csv, wind.csv, hydro.csv): ")
      val filename = scala.io.StdIn.readLine()
      FileIO.readFile(s"$dataDir/$filename") match {
        case Right(content) => println(content)
        case Left(err) => println(err)
      }
    case _ => println("Invalid option.")
  }

  def getAndSave(source: String, fetcher: (String, String) => Either[String, String]): Unit = {
    println("Enter start date (YYYY-MM-DDTHH:MM:SSZ): ")
    val start = scala.io.StdIn.readLine()
    println("Enter end date (YYYY-MM-DDTHH:MM:SSZ): ")
    val end = scala.io.StdIn.readLine()

    fetcher(start, end) match {
      case Right(csvData) =>
        val filename = s"$dataDir/${source.toLowerCase}.csv"
        FileIO.writeToFile(csvData, filename) match {
          case Right(_) => println(s"$source data saved to $filename")
          case Left(err) => println(err)
        }
      case Left(err) => println(err)
    }
  }

  mainLoop()
}