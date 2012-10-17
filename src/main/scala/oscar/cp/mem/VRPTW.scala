package oscar.cp.mem

import scala.collection.mutable.Queue
import scala.util.Random.nextFloat
import scala.util.Random.nextInt
import scala.Math.max
import scala.Math.pow
import scala.Math.atan2
import scala.Math.Pi

import oscar.util._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.VRPTWParser.parse
import oscar.visual.VisualTour
import oscar.search.IDSSearchController
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.visu.VisualRelax
import oscar.visual.VisualPareto
import oscar.cp.mem.visu.VisualRelax

/** VRPTW
 * 
 * 	@author Renaud Hartert - ren.hartert@gmail.com
 */

object VRPTW extends App {
	
	val instance = parse("data/VRPTW/Solomon/R101.txt")
	
	val hyp = scala.collection.mutable.Queue[Double]()
	
	// Data
	val nCustomers = instance.n	
	val nVehicles  = 20//instance.k	
	val nSites     = nCustomers + nVehicles	
	val capacity   = instance.c	
	
	val Vehicles   = 0 until nVehicles
	val Customers  = 0 until nCustomers
	val Sites      = 0 until nCustomers + nVehicles
	val Depots     = nCustomers until nCustomers + nVehicles
	
	val demand  = new Array[Int](nSites) // Demand of each customer
	val twStart = new Array[Int](nSites) // Earliest delivery time of each customer
	val twEnd   = new Array[Int](nSites) // Latest delivery time of each customer
	val servDur = new Array[Int](nSites) // Duration needed to serve each customer
	
	val coord   = new Array[(Int, Int)](nSites)

	for(s <- Sites) {
		val i = if (s >= nCustomers) 0 else s+1
		
		demand(s)  = instance.demand(i)
		twStart(s) = instance.twStart(i) * 100
		twEnd(s)   = instance.twEnd(i) * 100
		servDur(s) = instance.servDur(i) * 100
		
		coord(s)   = instance.coord(i)
	}
	
	// Distance matrix between sites
	val dist = new Array[Array[Int]](nSites, nSites) 
	val realDist = new Array[Array[Double]](nSites, nSites) 
	
	for(c1 <- Sites; c2 <- Sites) {
		val i = if (c1 >= nCustomers) 0 else c1+1
		val j = if (c2 >= nCustomers) 0 else c2+1
		dist(c1)(c2) = (instance.dist(i)(j) * 100).toInt
		realDist(c1)(c2) = (instance.dist(i)(j))
	}
	
	val Horizon = twStart(Depots.min) to twEnd(Depots.min)
	
	// Model
	val cp = CPSolver()
	
	val prev      = Array.fill(nSites)(CPVarInt(cp, Sites))		// Previously visited site
	val next      = Array.fill(nSites)(CPVarInt(cp, Sites))		// Previously visited site
	val routeOf   = Array.fill(nSites)(CPVarInt(cp, Vehicles))	// Route of each vehicle
	val arrival   = Array.fill(nSites)(CPVarInt(cp, Horizon)) 	// Date of service of each site
	val departure = Array.fill(nSites)(CPVarInt(cp, Horizon)) 	// Departure from each site
	
	val load = Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))
	val tardiness = Array.fill(nCustomers)(CPVarInt(cp, Horizon))
	
	val totDist = CPVarInt(cp, 0 to dist.flatten.sum)
	val totTard = CPVarInt(cp, Horizon)
	
	// ------------------------------------------------------------------------
	// LNS BLOCK
	// ------------------------------------------------------------------------
	
	type Sol = Array[Int]
	
	var bestPrev  : Sol = new Sol(nSites)
	var bestNext  : Sol = new Sol(nSites)
	var bestRoute : Sol = new Sol(nSites)
	var bestDist  = 0
	
	cp.sc = new IDSSearchController(cp, 4)
	
	cp.lns(500, 3000) { 
		
		val nextRelax = nextInt(2) // Not Shaw
		
		relaxVariables(nextRelax match {
			case 0 => catd(10)
			case 1 => casd(20)
			case 2 => shaw(30, 10)
		})
	}
	
	// ------------------------------------------------------------------------
	// PREPROCESSING AND USEFUL FUNCTIONS
	// ------------------------------------------------------------------------
	
	val sortedCustomersByTwStart = Customers.sortBy(tw => tw)
	
	val angles = Array.tabulate(nCustomers)(i => {
		
		val x = coord(i)._1 - coord(Depots.min)._1
		val y = coord(i)._2 - coord(Depots.min)._2
		
		val theta = (atan2(x, y)*180/Pi).toInt
		if (theta < 0) (theta + 360) else theta	
	})
		
	// Normalized distances
	val maxDist = dist.map(_.max).max
	val nDist   = dist.map(_.map(_ / maxDist.toDouble))
	
	def relatedness(i : Int, j :Int) = {
		
		val v = if (bestPrev(i) == bestPrev(j)) 0 else 1
		1 / (nDist(i)(j) + v)
	}
		
	def adaptedAngle(angle : Int, alpha : Int) = {	
		
		val newAngle = angle - alpha 
		if (newAngle < 0) 360-(alpha-angle) else newAngle
	}
	
	def solFound {		
		
		bestPrev  = buildPrev
		bestRoute = buildRoute	
		bestNext  = buildNext
		bestDist  = totDist.value
	}
	
	def buildPrev  : Sol = prev.map(_.value)
	def buildNext  : Sol = next.map(_.value)
	def buildRoute : Sol = routeOf.map(_.value)
	
	// ------------------------------------------------------------------------
	// RELAXATION PROCEDURES
	// ------------------------------------------------------------------------
	
	def shaw(p : Int, beta : Int) : Array[Boolean] = {
		
		// Random selection of a customer	
		val selected = Array.fill(nSites)(false)
		selected(nextInt(nCustomers)) = true
		
		for (k <- 1 until p) {
			
			val selectedC  = Customers.filter(selected(_))
			val remainingC = Customers.filter(!selected(_))
			
			// Selects randomly a customer in S
			val c = selectedC(nextInt(selectedC.size))
			
			// Order by relatedness with c
			val sortedC = remainingC.sortWith((i,j) => relatedness(c, i) > relatedness(c, j))
			
			val r = sortedC((pow(nextFloat, beta) * sortedC.size).floor.toInt)
			selected(r) = true
		}
		
		selected
	}
	
	def casd(p : Int) : Array[Boolean] = {
		
		val routes   = Array.fill(nVehicles)(false)
		val selected = Array.fill(nSites)(false)
		
		val alpha = nextInt(360)
		val sortedC = Customers.sortBy(i => adaptedAngle(angles(i), alpha))
		
		for (i <- 0 until p)
			routes(bestRoute(sortedC(i))) = true
				
		for (i <- Customers) 
			if (routes(bestRoute(i))) 
				selected(i) = true
			
		selected
	}
	
	def catd(p : Int) : Array[Boolean] = {
		
		val routes   = Array.fill(nVehicles)(false)
		val selected = Array.fill(nSites)(false)
		
		// Ensures the relaxation of p customers (not depots)
		val max = nCustomers - p - 1
		val alpha = nextInt(twStart(sortedCustomersByTwStart(max)))

		// Keeps relevant customers
		val filteredC = Customers.filter(i => twStart(i) >= alpha)
		// Sorts relevant customers in order to select the p first
		val sortedC = filteredC.sortBy(i => twEnd(i))
		
		for (i <- 0 until p)
			routes(bestRoute(sortedC(i))) = true
				
		for (i <- Customers) 
			if (routes(bestRoute(i))) 
				selected(i) = true
			
		selected
	}
	
	def relaxVariables(selected : Array[Boolean]) {
		
		visu.update(bestPrev, bestNext, selected)
		
		val constraints : Queue[Constraint] = Queue()
		
		for (i <- Sites) {			
			if(!selected(i)) {
				
				constraints enqueue (routeOf(i) == bestRoute(i))	
				
				if (!selected(bestPrev(i)))
					constraints enqueue (prev(i) == bestPrev(i))					
				if (!selected(bestNext(i)))
					constraints enqueue (next(i) == bestNext(i))
			}				
		}	
		cp.post(constraints.toArray)
	}
	
	// ------------------------------------------------------------------------
	// VISUALIZATION
	// ------------------------------------------------------------------------
	
	val visu = new VisualRelax(coord, realDist)
	
	// ------------------------------------------------------------------------
	// CONSTRAINTS BLOCK
	// ------------------------------------------------------------------------

	cp.minimize(totDist, totTard) subjectTo {
		
		// Channeling
		for (i <- Sites) {
			cp.add(next(prev(i)) == i)
			cp.add(prev(next(i)) == i)
		}
		
		// TSP constraint
		cp.add(circuit(prev), Strong)
		cp.add(circuit(next), Strong)
		
		// Length of the cycle
		cp.add(sum(Sites)(i => dist(i)(prev(i))) == totDist)
		cp.add(sum(Sites)(i => dist(i)(next(i))) == totDist)
		
		// Tardiness
		for (i <- Customers) 
			cp.add(tardiness(i) == maximum(Array(CPVarInt(cp, 0), arrival(i) - twEnd(i))))
			
		// Total tardiness
		cp.add(sum(Customers)(i => tardiness(i)) == totTard)
		
		// Route consistency
		for (i <- Customers) 
			cp.add(routeOf(i) == routeOf(prev(i)))
			
		// All vehicle start in different depots
		for (i <- Depots)
			cp.add(routeOf(i) == i - nCustomers)
			
		// Capacity of vehicles
		cp.add(binpacking(routeOf, demand, load))
			
		// Delivery time
		for (i <- Sites) {
			
			// A vehicle can arrived before starting time 
			cp.add(arrival(i) == maximum(Array(CPVarInt(cp, twStart(i)), departure(prev(i)) + dist(i)(prev(i)))))
			cp.add(new TimeWindow(cp, prev(i), arrival(i), departure, dist(i), twStart(i)))
			
			// A vehicle must finish before end of the time-window
			cp.add(arrival(i) <= twEnd(i))
			
			// A vehicle must wait for start of the time-window before starting the service
			cp.add(arrival(i) >= twStart(i))
		}
		
		// A vehicle starts immediately after service
	    for(i <- Customers) {
			cp.add(departure(i) == arrival(i) + servDur(i))
	    }
			
		// All vehicle start on the morning 
		for(i <- Depots)
			cp.add(departure(i) == 0)
		
	} 
		
	// ------------------------------------------------------------------------
	// EXPLORATION BLOCK
	// ------------------------------------------------------------------------
	
	cp.exploration {
		
		while (!allBounds(prev)) {
		
			val firstDepot = max(Depots.min, maxVal(prev))
			
			val i = selectMin(Sites, i => !prev(i).isBound)(i => prev(i).size)
			val j = selectMin(Sites, j => prev(i).hasValue(j) && j <= firstDepot + 1)(j => dist(i)(j))
	
			cp.branch(cp.post(prev(i) == j))(cp.post(prev(i) != j))
		}
		
		/*while (!allBounds(prev)) {
			
			var x = -1
			var v = -1
			var maxMinDist = Int.MinValue
			
			for (i <- Sites; if (!prev(i).isBound)) {

				var minDist = Int.MaxValue
				var id = -1
				
				for (j <- Sites; if(prev(i).hasValue(j))) {
					if (dist(i)(j) < minDist) {
						minDist = dist(i)(j)
						id = j
					}
				}
				
				if (minDist > maxMinDist) {
					x = i
					v = id
					maxMinDist = minDist
				}
			}
			
			cp.branch(cp.post(prev(x) == v))(cp.post(prev(x) != v))
		}*/
		
		/*while (!allBounds(prev)) {
			
			var x = -1
			var maxRegret = Int.MinValue
			
			for (i <- Sites; if (!prev(i).isBound)) {

				var distK1 = Int.MaxValue
				var distK2 = Int.MaxValue
				
				for (j <- Sites; if(prev(i).hasValue(j))) {		
					
					if (dist(i)(j) < distK1) {
						distK2 = distK1
						distK1 = dist(i)(j)
					}
					else if (dist(i)(j) < distK2) {
						distK2 = dist(i)(j)
					}
				}
				
				val regret = distK2 - distK1
				
				if (regret > maxRegret) {
					x = i
					maxRegret = regret
				}
			}
			
			val v = selectMin(Sites, v => prev(x).hasValue(v))(v => dist(x)(v))
			
			cp.branch(cp.post(prev(x) == v))(cp.post(prev(x) != v))
		}*/
		
		solFound
	}
	
	println("\nFinished !")
	cp.printStats
	
	def selectMin(x : IndexedSeq[Int], b : (Int => Boolean))(f : (Int => Int)) : Int = {
                        
	    val filtered = x.filter(i => b(i))
	    val sorted   = filtered.sortBy(f)
	          
	    if (sorted.size == 0) {
	    	cp.fail()
	    	-1
	    } else sorted(0)
	}
	
	def maxVal(x : Array[CPVarInt]) = {
		var max = Int.MinValue
		for (i <- 0 until x.size)
			if (x(i).isBound && x(i).value > max)
				max = x(i).value
		max
	}
}