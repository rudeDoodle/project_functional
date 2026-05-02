// Main application starting point
object Main extends App {
  // Ensure data directory exists first
  val dataDir = "data"
  val dir = new java.io.File(dataDir)
  if (!dir.exists()) dir.mkdirs()

  // Function to display the main menu
  private def printMenu(): Unit = {
    println("\n--- Renewable Energy Plant System ---")
    println("1. Fetch Solar Data")
    println("2. Fetch Wind Data")
    println("3. Fetch Hydro Data")
    println("4. View Raw Data (File Reader)")
    println("5. Run Data Analysis (Stats, Sort, Filter)")
    println("6. Plant Overview (Generation & Storage)")
    println("7. Exit")
  }

  // Main loop to handle user interaction
  private def mainLoop(): Unit = {
    printMenu()
    print("Choose option: ")
    val choice = scala.io.StdIn.readLine()

    choice match {
      case "7" => println("Goodbye.")
      case _ =>
        handleChoice(choice)
        mainLoop()
    }
  }

  //Function to handle all different choices
  private def handleChoice(choice: String): Unit = choice match {
    case "1" => getAndSave("Solar", GetData.fetchSolar)
    case "2" => getAndSave("Wind", GetData.fetchWind)
    case "3" => getAndSave("Hydro", GetData.fetchHydro)
    case "4" => viewRawData()
    case "5" => runAnalysisWorkflow()
    case "6" => plantOverview()
    case _   => println("Invalid option.")
  }

  // Function for option 4 - view raw data from file
  private def viewRawData(): Unit = {
    print("Enter filename to view: ")
    val filename = scala.io.StdIn.readLine()
    FileIO.readFile(s"$dataDir/$filename") match {
      case Right(content) => println(s"\n--- Raw Content ---\n$content")
      // Return an error message if file cannot be read
      case Left(err) => println(err)
    }
  }

  // Function for option 5 - run analysis workflow
  private def runAnalysisWorkflow(): Unit = {
    print("Analyze which file? (solar.csv, wind.csv, hydro.csv): ")
    val filename = scala.io.StdIn.readLine()

    FileIO.readFile(s"$dataDir/$filename") match {
      case Right(content) =>
        // If file can be read, parse the data
        val records = RenewableSystem.parseRawData(content)

        if (records.isEmpty) {
          println("No data found or file format incorrect.")
        } else {
          // If data is found, present analysis options by time period or value range
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
              // If the user selects to search by value range, request for minimum and maximum values
              print("Enter minimum value: ")
              val min = scala.io.StdIn.readDouble()
              print("Enter maximum value: ")
              val max = scala.io.StdIn.readDouble()

              // Search for records that fall within the specified value range and display them
              val found = RenewableSystem.searchByValue(records, min, max)
              println(s"\nFound ${found.size} records in range:")
              found.foreach(r => println(s"Time: ${r.startTime} | Value: ${r.value}"))

            // If user selects any analysis mode by time period, filter the records accordingly
            case m if List("1", "2", "3", "4").contains(m) =>
              val period = m match {
                case "1" => "hourly"
                case "2" => "daily"
                case "3" => "weekly"
                case "4" => "monthly"
              }
              // Perform the analysis from "RenewableSystem" for the appropriate time period and display the results
              val groups = RenewableSystem.filterBy(records, period)
              groups.foreach { case (timeLabel, data) =>
                println(s"\n--- Period: $timeLabel ---")
                RenewableSystem.analyze(data)
              }

            // In case of invalid analysis mode, display an error message
            case _ => println("Invalid analysis mode.")
          }
        }
      // In case of failure reading the file, display an error message
      case Left(err) => println(s"Error: $err")
    }
  }

  // Function for option 6 - plant overview
  private def plantOverview(): Unit = {
    val sources = List("solar", "wind", "hydro")

    val allRecords = sources.flatMap { src =>
      FileIO.readFile(s"$dataDir/$src.csv") match {
        case Right(content) => RenewableSystem.parseRawData(content)
        case Left(_) => Nil
      }
    }

    if (allRecords.isEmpty) {
      // If no data files are found, display a message prompting the user to fetch the data first
      println("\nNo data files found. Fetch data first (options 1-3).")
    } else {
      /* If data is found, display a comprehensive overview of it, including a multitude
       of statistics and insights for each energy source */

      // Display a header for the overview section
      println("\n" + "=" * 55)
      println("         RENEWABLE ENERGY PLANT OVERVIEW")
      println("=" * 55)

      val grouped = allRecords.groupBy(_.sourceType)

      // Display the statistics
      grouped.foreach { case (source, records) =>
        val sorted = records.sortBy(_.startTime.toString)
        val total = records.map(_.value).sum
        val avg = total / records.size
        val latest = sorted.last
        val peak = records.maxBy(_.value)
        val lowest = records.minBy(_.value)
        val timespan = s"${sorted.head.startTime.toLocalDate} to ${sorted.last.startTime.toLocalDate}"

        println(s"\n--- $source (Dataset ${records.head.datasetId}) ---")
        println(f"  Period:          $timespan")
        println(f"  Total Records:   ${records.size}")
        println(f"  Total Generated: $total%.2f MW")
        println(f"  Average Output:  $avg%.2f MW")
        println(f"  Peak Output:     ${peak.value}%.2f MW  (${peak.startTime})")
        println(f"  Lowest Output:   ${lowest.value}%.2f MW  (${lowest.startTime})")
        println(f"  Latest Reading:  ${latest.value}%.2f MW  (${latest.startTime})")

        // Check for and display alerts based on the latest reading
        RenewableSystem.checkAlerts(records).foreach { alert =>
          println(s"  [ALERT] $alert")
        }
      }

      /* Display a closure header for the overview section as well as the sum of
      all energy generated across all sources and records
      */
      val grandTotal = allRecords.map(_.value).sum
      println("\n" + "-" * 55)
      println(f"  Combined Generation: $grandTotal%.2f MW across ${allRecords.size} records")
      println("=" * 55)
    }
  }

  // Function used in options 1-3 to fetch data and save it to a file
  private def getAndSave(source: String, fetcher: (String, String) => Either[String, String]): Unit = {
    // Prompt the user to insert start and end dates for the data request
    println(s"Requesting $source data...")
    println("Start Date (YYYY-MM-DDTHH:MM:SSZ): ")
    val start = scala.io.StdIn.readLine()
    println("End Date (YYYY-MM-DDTHH:MM:SSZ): ")
    val end = scala.io.StdIn.readLine()

    fetcher(start, end) match {
      case Right(csvData) =>
        val filename = s"$dataDir/${source.toLowerCase}.csv"
        val wrapped = s"""$csvData"""
        FileIO.writeToFile(wrapped, filename) match {
          case Right(_) => println(s"Success! Saved to $filename")
          // In case of failure to write the file, display an error message
          case Left(err) => println(err)
        }
      // In case of failure to fetch the data, display an error message
      case Left(err) => println(s"Fetch failed: $err")
    }
  }

  mainLoop()
}