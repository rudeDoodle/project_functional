object Main extends App {
  val dataDir = "data"
  val dir = new java.io.File(dataDir)
  if (!dir.exists()) dir.mkdirs()

  def printMenu(): Unit = {
    println("\n--- Renewable Energy Plant System ---")
    println("1. Fetch Solar Data")
    println("2. Fetch Wind Data")
    println("3. Fetch Hydro Data")
    println("4. View Raw Data (File Reader)")
    println("5. Run Data Analysis (Stats, Sort, Filter)") // The new option
    println("6. Exit")
  }

  def mainLoop(): Unit = {
    printMenu()
    print("Choose option: ")
    val choice = scala.io.StdIn.readLine()

    choice match {
      case "6" => println("Goodbye.")
      case _ =>
        handleChoice(choice)
        mainLoop()
    }
  }

  def handleChoice(choice: String): Unit = choice match {
    case "1" => getAndSave("Solar", GetData.fetchSolar)
    case "2" => getAndSave("Wind", GetData.fetchWind)
    case "3" => getAndSave("Hydro", GetData.fetchHydro)
    case "4" => viewRawData()
    case "5" => runAnalysisWorkflow()
    case _   => println("Invalid option.")
  }
  
  def viewRawData(): Unit = {
    print("Enter filename to view: ")
    val filename = scala.io.StdIn.readLine()
    FileIO.readFile(s"$dataDir/$filename") match {
      case Right(content) => println(s"\n--- Raw Content ---\n$content")
      case Left(err) => println(err)
    }
  }
  
  def runAnalysisWorkflow(): Unit = {
    print("Analyze which file? (solar.csv, wind.csv, hydro.csv): ")
    val filename = scala.io.StdIn.readLine()

    FileIO.readFile(s"$dataDir/$filename") match {
      case Right(content) =>
        val records = RenewableSystem.parseRawData(content)

        if (records.isEmpty) {
          println("No data found or file format incorrect.")
        } else {
          println(s"\n[System] Found ${records.size} records.")
          println("Select Analysis Mode:")
          println("1. Hourly Analysis")
          println("2. Daily Analysis")
          println("3. Weekly Analysis")
          println("4. Monthly Analysis")
          println("5. Search by Value Range")

          val mode = scala.io.StdIn.readLine()

          mode match {
            case "5" =>
              print("Enter minimum value: ")
              val min = scala.io.StdIn.readDouble()
              print("Enter maximum value: ")
              val max = scala.io.StdIn.readDouble()
              val found = RenewableSystem.searchByValue(records, min, max)
              println(s"\nFound ${found.size} records in range:")
              found.foreach(r => println(s"Time: ${r.startTime} | Value: ${r.value}"))

            case m if List("1", "2", "3", "4").contains(m) =>
              val period = m match {
                case "1" => "hourly"
                case "2" => "daily"
                case "3" => "weekly"
                case "4" => "monthly"
              }
              val groups = RenewableSystem.filterBy(records, period)
              groups.foreach { case (timeLabel, data) =>
                println(s"\n--- Period: $timeLabel ---")
                RenewableSystem.analyze(data)
              }

            case _ => println("Invalid analysis mode.")
          }
        }
      case Left(err) => println(s"Error: $err")
    }
  }

  def getAndSave(source: String, fetcher: (String, String) => Either[String, String]): Unit = {
    println(s"Requesting $source data...")
    println("Start Date (YYYY-MM-DDTHH:MM:SSZ): ")
    val start = scala.io.StdIn.readLine()
    println("End Date (YYYY-MM-DDTHH:MM:SSZ): ")
    val end = scala.io.StdIn.readLine()

    fetcher(start, end) match {
      case Right(csvData) =>
        val filename = s"$dataDir/${source.toLowerCase}.csv"
        val wrapped = s"""{"data":"$csvData"}"""
        FileIO.writeToFile(wrapped, filename) match {
          case Right(_) => println(s"Success! Saved to $filename")
          case Left(err) => println(err)
        }
      case Left(err) => println(s"Fetch failed: $err")
    }
  }

  mainLoop()
}