package oscar.cp.mem.vrptw

import scala.io.Source
import scala.math.sqrt
import scala.math.pow

object VRPTWParser {
  
  private val scale = 1
  
  case class InstanceVRPTW(
    nCustomers: Int, 
    nVehicles: Int, 
    capacity: Int, 
    demand: Array[Int], 
    twStart: Array[Int], 
    twEnd: Array[Int], 
    servDur: Array[Int], 
    dist: Array[Array[Double]], 
    coord: Array[(Int, Int)]
  )

  def parse(filepath: String, depotsFactor: Int = 1): InstanceVRPTW = {

    var lines = Source.fromFile(filepath).getLines.toList
    val name = lines.head.trim
    lines = lines.drop(4)

    var l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray   
    val nVehicles = l(0).toInt
    val capacity = l(1).toInt

    lines = lines.drop(5)

    val nCustomers = lines.size - 1
    val nSites = nCustomers + nVehicles*depotsFactor

    val coord = Array.fill(nSites)((0, 0))
    val demand = Array.fill(nSites)(0)
    val twStart = Array.fill(nSites)(0)
    val twEnd = Array.fill(nSites)(0)
    val servDur = Array.fill(nSites)(0)
    
    // Depots
    l = lines.head.trim.split("[ ,\t]+").map(_.toInt).toArray
    for (i <- nCustomers until nSites) {
      coord(i) = (l(1) * scale, l(2) * scale)
      demand(i) = l(3)
      twStart(i) = l(4) * scale
      twEnd(i) = l(5) * scale
      servDur(i) = l(6) * scale
    }
    lines = lines.drop(1)
    
    // Customers 
    for (i <- 0 until nCustomers) {
      val l = lines.head.trim.split("[ ,\t]+").map(_.toInt).toArray
      coord(i) = (l(1) * scale, l(2) * scale)
      demand(i) = l(3)
      twStart(i) = l(4) * scale
      twEnd(i) = l(5) * scale
      servDur(i) = l(6) * scale
      lines = lines.drop(1)
    }
    
    val distMatrix = Array.tabulate(nSites, nSites)((i, j) => {
      val (xi, yi) = coord(i)
      val (xj, yj) = coord(j)
      sqrt(pow(xi - xj, 2) + pow(yi - yj, 2))
    })

    InstanceVRPTW(nCustomers, nVehicles, capacity, demand, twStart, twEnd, servDur, distMatrix, coord)
  }
}