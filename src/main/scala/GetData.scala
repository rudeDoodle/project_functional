import java.net.{HttpURLConnection, URL}
import scala.io.Source
object GetData {
  val apiKey = sys.env.getOrElse("FINGRID_API_KEY","")
  val  baseUrl = "https://data.fingrid.fi/api/datasets"
def fetchData(dataID: Int, startTime: String, endTime: String):Either[String,String] = {
  val urlStr = s"$baseUrl/$dataID/data?startTime=$startTime&endTime=$endTime&format=csv&sortBy=startTime&sortOrder=asc"
  try {
    var url = new URL(urlStr)
    val connection = url.openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("GET")
    connection.setRequestProperty("x-api-key", apiKey)
    val responseCode = connection.getResponseCode
    if (responseCode == 200) {
      val source = Source.fromInputStream(connection.getInputStream)
      val body = source.mkString
      source.close()
      Right(body)
    } else if (responseCode == 401) {
      Left("Invalid API key. Check your FINGRID_API_KEY.")
    } else if (responseCode == 404) {
      Left("Dataset not found.")
    } else {
      Left(s"API error: HTTP $responseCode")
    }
  } catch{ case e: java.net.UnknownHostException => Left("No Internet connection")
  case e: java.net.SocketTimeoutException => Left("API request timed out")
  case e: Exception => Left(s"Connection error: ${e.getMessage}")
  }
}

def fetchWind(start:String, end: String): Either[String, String] = fetchData(75, start,end)
def fetchSolar(start: String, end: String): Either[String,String] = fetchData(248,start,end)
def fetchHydro(start: String, end: String): Either[String,String] = fetchData(191,start,end)

}
