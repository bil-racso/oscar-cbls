package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.visual._

import scala.collection.JavaConversions._
import scala.io.Source
import java.lang._

class VRPTW_LNS extends CPModel {

	def main(args: Array[String]) {

		val lines = Source.fromFile("data/tsp.txt").getLines.toList
    
	    val n = 100 // Number of customers to visit
	    val k = 10  // Number of vehicles
	    val q = 200 // Capacity of the vehicles
	    
	    val customers = (0 to n)
	    val depots    = (0 to k)
	    val customersAndDepots = (0 to n+k)
	    val horizon   = 10000
	    
	    val demand = (0 to 1)
	    val twStart = (0 to 1)
	    val twEnd = (0 to 1)
	    val service = (0 to 1)
	    
	    val distance = lines.grouped(n).map(i => i.map(j => j.toInt).toArray).toArray
	    
	    // Modeling
	    val cp = new CPSolver()
	    
	    // Variables
		val previous     = Array.fill(customersAndDepots.size)(CPVarInt(cp, customersAndDepots))
		val routeOf      = Array.fill(customersAndDepots.size)(CPVarInt(cp, customersAndDepots))
	    val serviceStart = Array.fill(customersAndDepots.size)(CPVarInt(cp, customersAndDepots))
	    val departure    = Array.fill(customersAndDepots.size)(CPVarInt(cp, customersAndDepots))
	    val totDist      = Array.fill(customersAndDepots.size)(CPVarInt(cp, customersAndDepots))
	    
	    val dist = CPVarInt(cp, 0 to distance.flatten.sum)
	
	    cp.minimize(dist) subjectTo {
	      
			//for (i <- customersAndDepots)
				//cp.add(routeOf(i) === routeOf(previous(i)))
	      
	        
	      //cp.add(circuit(succ), Strong) //ask to have a strong filtering
	     // cp.add(sum(Cities)(i => element(distMatrix(i), succ(i))) == dist)
	      
	    } exploration {
	      /*
	      //exploration of the search tree
	      while (!allBounds(succ)) {
	         val res = minDomNotbound(succ)
	         val (x, i) = res.first
	         // get the closest successor in the domain of x
	         val v = argMin((x.getMin() to x.getMax()).filter(x.hasValue(_)))(distMatrix(i)(_)).first
	         cp.branch(cp.post(x == v)) (cp.post(x != v))
	      }*/
	    }
	
	    cp.printStats()
	}
  
	def parse(file : String) {
		
	}
}
