package oscar.visual

class Location(val lat: Double, val lon: Double) {
  
  private def degToRadian(deg: Double): Double = deg*math.Pi / 180.0

  private def radianToDeg(rad: Double): Double = rad*180.0 / math.Pi
  
  def distance(location: Location): Double = {
    val lat1 = lat
    val lon1 = lon
    val lat2 = location.lat
    val lon2 = location.lon
    val theta = lon1 - lon2
    var dist = math.sin(degToRadian(lat1)) * math.sin(degToRadian(lat2)) + math.cos(degToRadian(lat1)) * math.cos(degToRadian(lat2)) * math.cos(degToRadian(theta));
    dist = math.acos(dist)
    dist = radianToDeg(dist)
    dist = dist * 60 * 1.1515
    dist = dist * 1.609344
    dist
  }

  override def toString: String = "Lat: " + lat + ", Lon: " + lon
}

object Location {
  
  def apply(lat: Double, lon: Double): Location = new Location(lat, lon)
  
  def apply(lat: String, lon: String): Location = new Location(lat.toDouble, lon.toDouble)
}