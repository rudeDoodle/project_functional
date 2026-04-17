import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.util.Try

case class RenewableRecord(
                            sourceType: String,
                            datasetId: Int,
                            startTime: ZonedDateTime,
                            endTime: ZonedDateTime,
                            value: Double
                          )

object RenewableSystem {

  def identifySource(id: Int): String = id match {
    case 191 => "Hydro"
    case 75 => "Wind"
    case 248 => "Solar"
    case _ => "Unknown Renewable"
  }

  def parseRawData(jsonInput: String): Seq[RenewableRecord] = {
    try {
      // 1. Isolate the part between "data":" and the next "
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

    } catch {
      case e: Exception =>
        println(s"Error parsing one-line data: ${e.getMessage}")
        Nil
    }
  }


  def searchByValue(records: Seq[RenewableRecord], min: Double, max: Double): Seq[RenewableRecord] = {
    records.filter(r => r.value >= min && r.value <= max)
  }

  def sortByValue(records: Seq[RenewableRecord], ascending: Boolean = true): Seq[RenewableRecord] = {
    if (ascending) records.sortBy(_.value)
    else records.sortBy(_.value)(Ordering[Double].reverse)
  }


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
    println(f"Records:  $size")
    println(f"Mean:     $mean%.2f")
    println(f"Median:   $median%.2f")
    println(f"Mode:     $mode%.2f")
    println(f"Range:    $range%.2f")
    println(f"Midrange: $midrange%.2f")
    println("-" * 40)
  }

  def filterBy(records: Seq[RenewableRecord], timeframe: String): Map[String, Seq[RenewableRecord]] = {
    records.groupBy { r =>
      timeframe.toLowerCase match {
        case "hourly" => r.startTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:00"))
        case "daily" => r.startTime.toLocalDate.toString
        case "weekly" => s"Week ${r.startTime.get(java.time.temporal.IsoFields.WEEK_OF_WEEK_BASED_YEAR)}"
        case "monthly" => r.startTime.getMonth.toString
        case _ => "All"
      }
    }
  }

}