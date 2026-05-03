// Impoer necessary libraries for handling date and time, and for safely parsing data
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

// Case class to represent a single record of renewable energy data
case class RenewableRecord(
                            sourceType: String,
                            datasetId: Int,
                            startTime: ZonedDateTime,
                            endTime: ZonedDateTime,
                            value: Double
                          )

// Object for handling all operations related to renewable energy data
object RenewableSystem {

  // Function to identify the energy source based on the dataset ID
  private def identifySource(id: Int): String = id match {
    case 191 => "Hydro"
    case 75 => "Wind"
    case 248 => "Solar"
    case _ => "Unknown Renewable"
  }

  // Function for parsing data from the raw JSON string and converting it into a sequence of RenewableRecord objects
  def parseRawData(jsonInput: String): Seq[RenewableRecord] = {
    try {

      // Manually extract the "data" field from the JSON string
      val startTag = "\"data\":\""
      val startIndex = jsonInput.indexOf(startTag)

      if (startIndex == -1) return Nil

      val actualStart = startIndex + startTag.length
      val endIndex = jsonInput.indexOf("\"", actualStart)
      if (endIndex == -1) return Nil

      val rawContent = jsonInput.substring(actualStart, endIndex)


      val cleanContent = rawContent.replace("\\n", "\n")


      val lines = cleanContent.split("\n")
        .map(_.trim)
        .filter(_.nonEmpty)
        .filterNot(l => l.startsWith("datasetId"))

      // Convert each line into a RenewableRecord
      lines.flatMap { line =>
        Try {
          val cols = line.split(";")
          RenewableRecord(
            sourceType = identifySource(cols(0).toInt),
            datasetId = cols(0).toInt,
            startTime = ZonedDateTime.parse(cols(1)),
            endTime = ZonedDateTime.parse(cols(2)),
            value = cols(3).toDouble
          )
        }.toOption
      }.toSeq

    // Handle errors relating to parsing the JSON string or converting the data into records
    } catch {
      case e: Exception =>
        println(s"Error parsing one-line data: ${e.getMessage}")
        Nil
    }
  }

  // Function for searching records based on a value range; returns a sequence of appropriate records
  def searchByValue(records: Seq[RenewableRecord], min: Double, max: Double): Seq[RenewableRecord] = {
    records.filter(r => r.value >= min && r.value <= max)
  }

  // Function for sorting records by their value, ascending or descending
  private def sortByValue(records: Seq[RenewableRecord], ascending: Boolean = true): Seq[RenewableRecord] = {
    if (ascending) records.sortBy(_.value)
    else records.sortBy(_.value)(Ordering[Double].reverse)
  }

  // Analyze a sequence of records and calculate various statistics, then print them
  def analyze(records: Seq[RenewableRecord]): Unit = {
    if (records.isEmpty) return println("No data found.")

    val vals = records.map(_.value).sorted
    val size = vals.size

    val mean = vals.sum / size

    val median = if (size % 2 == 0) (vals(size / 2 - 1) + vals(size / 2)) / 2.0 else vals(size / 2)

    val mode = vals.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

    val range = vals.last - vals.head

    val midrange = (vals.last + vals.head) / 2.0

    println(f"Source: ${records.head.sourceType} (ID: ${records.head.datasetId})")
    println(f"Records:  $size  MW")
    println(f"Mean:     $mean%.2f MW")
    println(f"Median:   $median%.2f MW")
    println(f"Mode:     $mode%.2f MW")
    println(f"Range:    $range%.2f MW")
    println(f"Midrange: $midrange%.2f MW")
    println("-" * 40)
  }

  // Function for handling the grouping of records based on the specified time frame
  def filterBy(records: Seq[RenewableRecord], timeframe: String): Map[String, Seq[RenewableRecord]] = {
    records.groupBy { r =>
      timeframe.toLowerCase match {
        case "hourly" => r.startTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:00"))
        case "daily" => r.startTime.toLocalDate.toString
        case "weekly" => f"Week ${r.startTime.get(java.time.temporal.IsoFields.WEEK_OF_WEEK_BASED_YEAR)}%02d"
        case "monthly" => r.startTime.getMonth.toString
        case _ => "All"
      }
    }
  }

  /* Function that checks for any alert conditions in a sequence of records,
  returns a sequence of alert messages */
  def checkAlerts(records: Seq[RenewableRecord]): Seq[String] = {
    if (records.isEmpty) return Nil

    // Calculate the average value to use as a baseline for detecting anomalies
    val vals = records.map(_.value)
    val avg = vals.sum / vals.size

    // Define a low output threshold as 20% of the average value
    val lowThreshold = avg * 0.2

    // Find consecutive zero-output readings
    val zeroRuns = findZeroRuns(records)

    // Generate alerts for low output readings
    val lowOutputAlerts = records
      .filter(r => r.value < lowThreshold && r.value > 0)
      .take(3)
      .map(r => f"Low output detected: ${r.value}%.2f MW at ${r.startTime} (threshold: $lowThreshold%.2f MW)")

    // Generate an alert if there are consecutive zero-output readings
    val zeroAlerts = if (zeroRuns > 0) {
      Seq(s"Possible equipment malfunction: $zeroRuns consecutive zero-output readings detected")
    } else Nil

    // Detect sudden drops in energy output
    val dropAlerts = detectSuddenDrops(records, avg)

    // Combine all alerts into a single sequence, then return it
    lowOutputAlerts ++ zeroAlerts ++ dropAlerts
  }

  // Function for finding consecutive zero-output readings, which might indicate a malfunction
  private def findZeroRuns(records: Seq[RenewableRecord]): Int = {
    records
      .map(_.value)
      .foldLeft((0, 0)) { case ((maxRun, current), v) =>
        if (v == 0.0) (math.max(maxRun, current + 1), current + 1)
        else (maxRun, 0)
      }._1
  }

  // Function for detecting sudden drops in energy output
  private def detectSuddenDrops(records: Seq[RenewableRecord], avg: Double): Seq[String] = {
    val sorted = records.sortBy(_.startTime.toString)
    sorted.sliding(2).flatMap {
      case Seq(prev, curr) if prev.value > 0 && (prev.value - curr.value) / prev.value > 0.8 =>
        Some(f"Sudden drop: ${prev.value}%.2f -> ${curr.value}%.2f MW at ${curr.startTime}")
      case _ => None
    }.take(3).toSeq
  }

}