package oscar.cp.mem.tsp

import scala.io.Source

object TSPParser {
  
  def parseCoordinates(filepath: String): Array[(Int, Int)] = {
    var lines = Source.fromFile(filepath).getLines.toList
    lines = lines.drop(6)
    val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
    val n = lines.size - 1
    val coordinates = Array.tabulate(n)(i => {
      val l = lines.head.trim.split("[ ,\t]+").map(_.toInt).toArray
      val x = l(1)
      val y = l(2)
      lines = lines.drop(1)
      (x, y)
    })
    coordinates
  }
  
  def parseScull(filepath: String): Array[Array[Int]] = {
    null
  }

  def writeSet(filename: String, set: Array[(Int, Int)]) {
      
  }
  
  def writeScull(filename: String, set: Array[Array[Int]]) {
       
  }
}