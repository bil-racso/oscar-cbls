/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/
package oscar.visual

import java.net.URL
import scala.io.Source
import scala.collection.mutable.{ HashMap, SynchronizedMap }
import scala.xml._
import java.io.IOException;

/*
 * Representation of a point defined by coordinates
 */
case class MapPoint(lat : Double, long : Double) {
    override def toString() = lat+","+long
}

/*
 * Mapquest web api requests
 */
object MapQuest {
  val baseUrl = "http://open.mapquestapi.com/directions/v1/route?outFormat=xml&shapeFormat=raw&generalize=200"
  //private val cache = new HashMap[(MapPoint, MapPoint), List[MapPoint]] with SynchronizedMap[(MapPoint, MapPoint), List[MapPoint]]
  
	/*
	 * Perform the http request and extract path from response
	 */
    def getPath(orig: MapPoint, dest : MapPoint) : List[MapPoint] = 
    {
		  val xmldata = getRoute(orig, dest)
		  if((xmldata \ "statusCode").text != "0") throw new IOException ("Error retrieving path from mapquest");
		  else {
		    (xmldata \ "route" \ "shape" \ "shapePoints" \ "latLng").map(node => new MapPoint((node \ "lat").text.toDouble, (node \ "lng").text.toDouble)).toList
		  }
    }
  	
  	/*
  	 * Http request to get an xml element containing the path
  	 */
  	private def getRoute(orig: MapPoint, dest : MapPoint) : Elem = {
  	  val answer = (new URL(baseUrl + "&from=" + orig + "&to=" + dest)).openStream()
  	  XML.loadString(Source.fromInputStream(answer).mkString(""))
  	}
}


/*
 * Represent a path from orig to dest as a list of MapLines
 */
class MapPath(map : VisualMap, o: MapPoint, d : MapPoint) {
  
  var orig = o
  var dest = d
  var lines = getLines()
  
  
  /*
   * get mapquest path from orig to dest
   */
  def getLines() : List[MapLine] = {
 	val waypoints = MapQuest.getPath(orig, dest)
 	(for(i <- 0 until waypoints.length-2) yield new MapLine(map, waypoints(i).lat, waypoints(i).long, waypoints(i+1).lat, waypoints(i+1).long)).toList
  }
  
  // constructor without using MapPoint structure
  def this(map : VisualMap, origlat : Double, origlong: Double, destlat : Double, destlong : Double) = this(map, new MapPoint(origlat,origlong), new MapPoint(destlat, destlong)) 

  /*
   *  change destination
   */
  def setDest(dlat : Double, dlong : Double) = {
    if(dlat != dest.lat || dlong != dest.long) {
      dest = new MapPoint(dlat, dlong)
      lines = getLines()
    }      
  }
  
  /*
   *  change origin
   */
  def setOrig(olat : Double, olong : Double) =  {
    if(olat != orig.lat || olong != orig.long) {
      orig = new MapPoint(olat, olong)
      lines = getLines()
    }      
  }
  
}

object MapPath {
  
  def main(args : Array[String]) = 
  {
    println("hello test")
  }
  
}

