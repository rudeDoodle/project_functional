// Import necessary libraries for connecting to an Http based API and handling responses
import java.net.{HttpURLConnection, URL}
import scala.io.Source
object GetData {

  // Get the API key from environment variable, if not found return empty string
  val apiKey = sys.env.getOrElse("FINGRID_API_KEY","")
  val  baseUrl = "https://data.fingrid.fi/api/datasets"

  /* Function for fetching the data from the API, takes the dataset ID and
  time range as parameters*/
  private def fetchData(dataID: Int, startTime: String, endTime: String):Either[String,String] = {
    val urlStr = s"$baseUrl/$dataID/data?startTime=$startTime&endTime=$endTime&format=csv&sortBy=startTime&sortOrder=asc"
    try {
      var url = new URL(urlStr)
      val connection = url.openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection.setRequestProperty("x-api-key", apiKey)
      val responseCode = connection.getResponseCode
      // If the response code is 200, it means the request was successful
      if (responseCode == 200) {
        // Read the response body and return it as a string
        val source = Source.fromInputStream(connection.getInputStream)
        val body = source.mkString
        source.close()
        Right(body)
      }
      // If the response is 401, it means the API key is invalid, alert the user
      else if (responseCode == 401) {
        Left("Invalid API key. Check your FINGRID_API_KEY.")
      }
      // If the response is 404, it means the dataset was not found, alert the user
      else if (responseCode == 404) {
        Left("Dataset not found.")
      }
      // For any other response code, return a generic error message with the code
      else {
        Left(s"API error: HTTP $responseCode")
      }
  }
  // Handle errors that may occur during the connection
  catch{ case e: java.net.UnknownHostException => Left("No Internet connection")
  case e: java.net.SocketTimeoutException => Left("API request timed out")
  case e: Exception => Left(s"Connection error: ${e.getMessage}")
  }
}

  /* Functions for fetching all kinds of data, they call the generic fetchData function
  with their appropriate dataset ID*/
def fetchWind(start:String, end: String): Either[String, String] = fetchData(75, start,end)
def fetchSolar(start: String, end: String): Either[String,String] = fetchData(248,start,end)
def fetchHydro(start: String, end: String): Either[String,String] = fetchData(191,start,end)

}
