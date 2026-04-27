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
    println("5. Run Data Analysis (Stats, Sort, Filter)")
    println("6. Plant Overview (Generation & Storage)")
    println("7. Exit")
  }

  def mainLoop(): Unit = {
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

  def handleChoice(choice: String): Unit = choice match {
    case "1" => getAndSave("Solar", GetData.fetchSolar)
    case "2" => getAndSave("Wind", GetData.fetchWind)
    case "3" => getAndSave("Hydro", GetData.fetchHydro)
    case "4" => viewRawData()
    case "5" => runAnalysisWorkflow()
    case "6" => plantOverview()
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

  def plantOverview(): Unit = {
    val sources = List("solar", "wind", "hydro")

    val allRecords = sources.flatMap { src =>
      FileIO.readFile(s"$dataDir/$src.csv") match {
        case Right(content) => RenewableSystem.parseRawData(content)
        case Left(_) => Nil
      }
    }

    if (allRecords.isEmpty) {
      println("\nNo data files found. Fetch data first (options 1-3).")
    } else {
      println("\n" + "=" * 55)
      println("         RENEWABLE ENERGY PLANT OVERVIEW")
      println("=" * 55)

      val grouped = allRecords.groupBy(_.sourceType)

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

        RenewableSystem.checkAlerts(records).foreach { alert =>
          println(s"  [ALERT] $alert")
        }
      }

      val grandTotal = allRecords.map(_.value).sum
      println("\n" + "-" * 55)
      println(f"  Combined Generation: $grandTotal%.2f MW across ${allRecords.size} records")
      println("=" * 55)
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
        val wrapped = s"""$csvData"""
        FileIO.writeToFile(wrapped, filename) match {
          case Right(_) => println(s"Success! Saved to $filename")
          case Left(err) => println(err)
        }
      case Left(err) => println(s"Fetch failed: $err")
    }
  }

  mainLoop()
}