package oscar.cp.mem

import scala.Math.max
import scala.util.Random.nextFloat
import scala.util.Random.nextInt
import scala.Math.pow
import oscar.util._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.VRPTWParser.parse
import oscar.visual.VisualTour
import oscar.search.IDSSearchController

/** VRPTW
 * 
 * 	@author Renaud Hartert - ren.hartert@gmail.com
 */

object VRPTW extends App {
	
	val instance = parse("data/VRPTW/Solomon/R105.txt")
	
	// Data
	val nCustomers = instance.n	
	val nVehicles  = instance.k	
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
		twStart(s) = instance.twStart(i)
		twEnd(s)   = instance.twEnd(i)
		servDur(s) = instance.servDur(i)
		
		coord(s)   = instance.coord(i)
	}
	
	// Distance matrix between sites
	val dist = new Array[Array[Int]](nSites, nSites) 
	
	for(c1 <- Sites; c2 <- Sites) {
		val i = if (c1 >= nCustomers) 0 else c1+1
		val j = if (c2 >= nCustomers) 0 else c2+1
		dist(c1)(c2) = instance.dist(i)(j)
	}
	
	val Horizon = twStart(Depots.min) to twEnd(Depots.min)
	
	// Model
	val cp = CPSolver()
	
	val prev      = Array.fill(nSites)(CPVarInt(cp, Sites))		// Previously visited site
	val next      = Array.fill(nSites)(CPVarInt(cp, Sites))		// Previously visited site
	val routeOf   = Array.fill(nSites)(CPVarInt(cp, Vehicles))	// Route of each vehicle
	val service   = Array.fill(nSites)(CPVarInt(cp, Horizon)) 	// Date of service of each site
	val departure = Array.fill(nSites)(CPVarInt(cp, Horizon)) 	// Departure from each site
	
	val load = Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))
	
	val totDist = CPVarInt(cp, 0 to dist.flatten.sum)
	
	// Visualization
	// --------------------------------------------------
	
	val visu = new VisualTour(coord, prev, totDist, "VRPTW", 8, 6, routeOf, twStart, twEnd)
	
	// LNS
	// --------------------------------------------------
	
	//cp.sc = new IDSSearchController(cp, 4)
	
	// Normalized distances
	val maxDist = dist.map(_.max).max
	val nDist   = dist.map(_.map(_ / (0.0 +maxDist)))
	
	def relatedness(i : Int, j :Int) = {		
		val v = if (bestRouteOf(i) == bestRouteOf(j)) 0 else 1
		1 / (nDist(i)(j) + v)
	}
	
	val bestPrev    = new Array[Int](nSites)
	val bestRouteOf = new Array[Int](nSites)
	val selected    = new Array[Boolean](nSites)
	var bestSol     = Int.MaxValue
	var prevSol		= Int.MaxValue
	
	var stagnation  = 0
	
	val beta = 1
	var p = 15
	
	cp.lns(10000, 2000) {
		
		if (bestSol == prevSol) {
			stagnation += 1
			if (stagnation >= 20) {
				stagnation = 0
				p += 1
				if (p > 30) p = 30
				println("\np : "+p)
			}
		}
		else {
			p = 15
			prevSol = bestSol
			stagnation = 0
		}
		
		// Adaptable LNS
		if (!cp.isLastLNSRestartCompleted) {
			cp.failLimit = (cp.failLimit * 110)/100
		} else {
			cp.failLimit = max(10, (cp.failLimit * 90)/100)
		}
		
		// Random selection of customers		
		val S = Array.fill(nSites)(false)
		S(nextInt(nSites)) = true
		
		for (k <- 1 until p) {
			
			val selectedC  = Sites.filter(S(_))
			val remainingC = Sites.filter(!S(_))
			
			// Selects randomly a customer in S
			val c = selectedC(nextInt(selectedC.size))
			
			// Order by relatedness with c
			val sortedC = remainingC.sortWith((i,j) => relatedness(c, i) <= relatedness(c, j))
			
			val r = sortedC((pow(nextFloat, beta) * sortedC.size).floor.toInt)
			S(r) = true
		}
		
		val filtered = Sites.filter(i => !S(i))
		
		val constraints1 : Array[Constraint] = filtered.map(i => prev(i) == bestPrev(i)).toArray
		val constraints2 : Array[Constraint] = filtered.map(i => routeOf(i) == bestRouteOf(i)).toArray

		cp.post(constraints1)
		cp.post(constraints2)
	}
	
	cp.minimize(totDist) subjectTo {
		
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
		
		// Route consistency
		for(i <- Customers) 
			cp.add(routeOf(i) == routeOf(prev(i)))
			
		// All vehicle start in different depots
		for(i <- Depots)
			cp.add(routeOf(i) == i - nCustomers)
			
		// Capacity of vehicles
		cp.add(binpacking(routeOf, demand, load))
			
		// Delivery time
		for (i <- Sites) {
			
			// A vehicle can arrived before starting time 
			cp.add(service(i) == maximum(Array(CPVarInt(cp, twStart(i)), departure(prev(i)) + dist(i)(prev(i)))))
			
			// A vehicle must finish before end of the time-window
			cp.add(service(i) <= twEnd(i))
			
			// A vehicle must wait for start of the time-window before starting the service
			cp.add(service(i) >= twStart(i))
		}
		
		// A vehicle starts immediately after service
	    for(i <- Customers) {
			cp.add(departure(i) == service(i) + servDur(i))
	    }
			
		// All vehicle start on the morning 
		for(i <- Depots)
			cp.add(departure(i) == 0)
		
	} exploration {
		
		var c = Depots.min
		
		/*while (!allBounds(prev)) {
			
			while(prev(c).isBound)
				c = prev(c).value
				
			if (prev(c).min >= Depots.min)
				cp.branch(cp.post(prev(c) == prev(c).min))(cp.post(prev(c) != prev(c).min))
									
			else {				
				val i = selectMin(Customers, j => prev(c).hasValue(j) )(j => dist(c)(j))
				cp.branch(cp.post(prev(c) == i))(cp.post(prev(c) != i))
			}
		}*/
		
		while (!allBounds(prev)) {
		
			val firstDepot = max(Depots.min, maxVal(prev))
			
			val i = selectMin(Sites, i => !prev(i).isBound)(i => prev(i).size)
			val j = selectMin(Sites, j => prev(i).hasValue(j) && j <= firstDepot + 1)(j => dist(i)(j))
	
			cp.branch(cp.post(prev(i) == j))(cp.post(prev(i) != j))
		}
					
		for(i <- Sites) {
			bestPrev(i) = prev(i).value
			bestRouteOf(i) = routeOf(i).value
		}
		
		bestSol = totDist.value
		
		visu.update
	}
	
	println("Finished !")
	cp.printStats
	
	def selectMin(x : Range, b : (Int => Boolean))(f : (Int => Int)) : Int = {
                        
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