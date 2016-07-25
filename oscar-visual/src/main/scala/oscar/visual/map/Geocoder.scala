package oscar.visual.map

import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL
import java.net.URLEncoder

import net.sf.json.JSONObject

object Geocoder {
  
  val ENCODING: String = "UTF-8"
  val KEY: String = ""
    
  def getLocation(address: String): Location = {
    val input = new BufferedReader(new InputStreamReader (new URL ("https://maps.googleapis.com/maps/api/geocode/json?address="+URLEncoder.encode(address, ENCODING)+"&key="+KEY).openStream ()))
    var location: Location = null
    val builder = new StringBuilder()
    var line:String = input.readLine()

    def checkRequest(status:String): Boolean ={
      status match{
        case "OK" =>
          println("Localisation request success !")
          true
        case "ZERO_RESULTS" =>
          println("Localisation request failed ! Please check the encoded address")
          false
        case "OVER_QUERY_LIMIT" =>
          println("You have exceeded your amount of request ! " +
            "Go on https://developers.google.com/maps/documentation/geocoding/usage-limits?hl=fr for more information about your quota.")
          false
        case "REQUEST_DENIED" =>
          println("Your request has been denied !")
          false
        case "INVALID_REQUEST" =>
          println("Something's wrong with your request, please check it.")
          false
        case "UNKNOWN_ERROR" =>
          println("Unknown error, it may be caused by the server please try again.")
          false
        case _ =>
          throw new Error("Unhandled status !!!!!")
      }
    }

    do{
      builder.append(line)
      line = input.readLine()
    }while (line != null)

    val requestResult:JSONObject = JSONObject.fromString(builder.toString())

    val status:String = requestResult.getString("status")

    if(checkRequest(status)){
      val results = requestResult.getJSONArray("results")
      val geometry = results.getJSONObject(0).getJSONObject("geometry")
      val locationJSON = geometry.getJSONObject("location")
      val (lat,long) = (locationJSON.getDouble("lat"),locationJSON.getDouble("lng"))
      location = new Location(lat,long)
    }

    location
  }

  def getDistances(address1:List[String], address2:List[String]): (Array[Array[Long]],Array[Array[Long]]) ={
    val distanceMatrix:Array[Array[Long]] = Array.tabulate(address1.size)(addr1 => Array.tabulate(address2.size)(addr2 => -1))
    val timeMatrix:Array[Array[Long]] = Array.tabulate(address1.size)(addr1 => Array.tabulate(address2.size)(addr2 => -1))

    /**
      * Due to the fact that the google distance matrix API limits usages of his service,
      * we have to divide the request into multiple ones.
      * The standard "account" allow us to make request containing at most 100 elements.
      * The number of elements is calculate like this : number of origins times number of destinations
      * So we limit the number of origins and destinations to 10
      */
    for(a1 <- 0 until Math.ceil(address1.size/10).toInt) {
      for(a2 <- 0 until Math.ceil(address2.size/10).toInt) {
        Thread.sleep(1500)
        val origins:String = address1
          .slice(a1*10,Math.min(address1.size,(a1+1)*10))
          .foldLeft("")((a,b) => if(a.isEmpty)a+URLEncoder.encode(b, ENCODING)else a+"|"+URLEncoder.encode(b, ENCODING))

        val destinations:String = address2
          .slice(a2*10,Math.min(address2.size,(a2+1)*10))
          .foldLeft("")((a,b) => if(a.isEmpty)a+URLEncoder.encode(b, ENCODING)else a+"|"+URLEncoder.encode(b, ENCODING))

        val input = new BufferedReader(new InputStreamReader (new URL ("https://maps.googleapis.com/maps/api/distancematrix/json?origins="+origins+"&destinations="+destinations+"&key="+KEY).openStream ()))

        val builder = new StringBuilder()
        var line: String = input.readLine()

        def checkResponse(status: String): Boolean = {
          status match {
            case "OK" =>
              println("Localisation request success !")
              true
            case "INVALID_REQUEST" =>
              println("Invalid request ! Please check the send request")
              false
            case "MAX_ELEMENTS_EXCEEDED" =>
              println("You have exceeded your amount of element per request ! " +
                "Go on https://developers.google.com/maps/documentation/geocoding/usage-limits?hl=fr for more information about your quota.")
              false
            case "OVER_QUERY_LIMIT" =>
              println("You have exceeded your amount of request ! " +
                "Go on https://developers.google.com/maps/documentation/geocoding/usage-limits?hl=fr for more information about your quota.")
              false
            case "REQUEST_DENIED" =>
              println("Something's wrong with your request, please check it.")
              false
            case "UNKNOWN_ERROR" =>
              println("Unknown error, it may be caused by the server please try again.")
              false
            case _ =>
              throw new Error("Unhandled status !!!!!")
          }
        }

        def checkInternalResponse(status: String, from: String, to: String): Boolean = {
          status match {
            case "OK" => true
            case "NOT_FOUND" =>
              println(from + " or " + to + " couldn't be found by the google services ! Please check the encoded addresses")
              false
            case "ZERO_RESULTS" =>
              println("No route could be found between " + from + " and " + to)
              false
            case _ =>
              throw new Error("Unhandled status !!!!!")
          }
        }

        do {
          builder.append(line)
          line = input.readLine()
        } while (line != null)

        val requestResult: JSONObject = JSONObject.fromString(builder.toString())
        val status: String = requestResult.getString("status")

        if (checkResponse(status)) {
          val rows = requestResult.getJSONArray("rows")
          for (i <- 0 until rows.length()) {
            val origin = rows.getJSONObject(i)
            val elements = origin.getJSONArray("elements")
            for (j <- 0 until elements.length()) {
              val destination = elements.getJSONObject(j)
              if (checkInternalResponse(destination.getString("status"), address1(i+(a1*10)), address2(j+(a2*10)))) {
                distanceMatrix(i+(a1*10))(j+(a2*10)) = destination.getJSONObject("distance").getInt("value")
                timeMatrix(i+(a1*10))(j+(a2*10)) = destination.getJSONObject("duration").getInt("value")
              }
            }
          }
        }
      }
    }
    (distanceMatrix,timeMatrix)
  }
}

object GeocoderExample extends App {
  val locations:List[String] = "Bruxelles"::"Liège"::"Mons"::"Charleroi"::Nil

  System.out.println("Getting Location ...")
  for(i <- locations.indices){
    System.out.println(Geocoder.getLocation(locations(i)))
  }
  Thread.sleep(1000)
  println("Getting Matrix ...")
  val (distanceMatrix,timeMatrix) = Geocoder.getDistances(locations.toList,locations.toList)
  for(from <- distanceMatrix.indices){
    for(to <- distanceMatrix(from).indices){
      println("Distance from " + locations(from) + " to " + locations(to) + " : " + distanceMatrix(from)(to))
      println("Travel time from " + locations(from) + " to " + locations(to) + " : " + timeMatrix(from)(to))
    }
  }


}
