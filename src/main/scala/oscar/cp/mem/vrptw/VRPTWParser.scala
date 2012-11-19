package oscar.cp.mem.vrptw

import scala.io.Source
import scala.Array.canBuildFrom

object VRPTWParser {

	def parse(filepath : String) : InstanceVRPTW = {
		
		var lines = Source.fromFile(filepath).getLines.toList

		val name = lines.head.trim
		
		lines = lines.drop(4)
		
		val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
		val k = l(0).toInt
		val c = l(1).toInt
		
		lines = lines.drop(5)
		
		
		val n = lines.size - 1
					
		val coord   = new Array[(Int, Int)](n+1)
		val demand  = new Array[Int](n+1)
		val twStart = new Array[Int](n+1)
		val twEnd   = new Array[Int](n+1)
		val servDur = new Array[Int](n+1)
	
		for(i <- 0 to n) {
	
			val l = lines.head.trim.split("[ ,\t]+").map(_.toInt).toArray
	
			coord(i)   = (l(1), l(2))
			demand(i)  = l(3)
			twStart(i) = l(4)
			twEnd(i)   = l(5)
			servDur(i) = l(6)
			
			lines = lines.drop(1)
		}
		
		val dist = new Array[Array[Double]](n+1, n+1)
		
		for(c1 <- 0 to n; c2 <- 0 until c1) {
			
			dist(c1)(c2) = (sqrt( pow(coord(c1)._1 - coord(c2)._1,2) + pow(coord(c1)._2 - coord(c2)._2,2) ))
			dist(c2)(c1) = dist(c1)(c2)
			dist(c1)(c1) = 0
			dist(c2)(c2) = 0
		}
		
		new InstanceVRPTW(n, k, c, demand, twStart, twEnd, servDur, dist, coord)
	}
}

case class InstanceVRPTW(n : Int, k : Int, c : Int, demand : Array[Int], twStart : Array[Int], twEnd : Array[Int], servDur : Array[Int], dist : Array[Array[Double]], coord : Array[(Int, Int)])
						 
