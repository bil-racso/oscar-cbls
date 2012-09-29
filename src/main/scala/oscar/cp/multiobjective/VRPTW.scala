package oscar.cp.multiobjective

import scala.Math.max

import oscar.cp.modeling._
import oscar.cp.core._

/** VRPTW
 * 
 * 	@author Renaud Hartert - ren.hartert@gmail.com
 */

object VRPTW extends App {
	
	val instance = VRPTWParser.parse("data/VRPTW/Solomon/C101.txt")
	
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

	for(s <- Sites) {
		val i = if (s > nCustomers) 0 else s
		demand(s)  = instance.demand(i) 
		twStart(s) = instance.twStart(i)
		twEnd(s)   = instance.twEnd(i)
		servDur(s) = instance.servDur(i)
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
	
	val prev      : Array[CPVarInt] = for(i <- Sites) yield CPVarInt(cp, Sites)	   // Previously visited site
	val routeOf   : Array[CPVarInt] = for(i <- Sites) yield CPVarInt(cp, Vehicles) // Route of each vehicle
	val service   : Array[CPVarInt] = for(i <- Sites) yield CPVarInt(cp, Horizon)  // Date of service of each site
	val departure : Array[CPVarInt] = for(i <- Sites) yield CPVarInt(cp, Horizon)  // Departure from each site
	
	val totDist = CPVarInt(cp, 0 to dist.flatten.sum)
	
	cp.minimize(totDist) subjectTo {
		
		// TSP constraint
		cp.add(circuit(prev), Strong)
		
		// Route consistency
		for(i <- Customers) 
			cp.add(routeOf(i) == routeOf(prev(i)))
			
		// All vehicle start in different depots
		for(i <- Depots)
			cp.add(routeOf(i) == i - nCustomers)
			
		// Capacity of vehicles
		cp.add(binpacking(routeOf, demand, Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))))
			
		// Delivery time
		for (i <- Sites) {
			
			// A vehicle can arrived before starting time 
			cp.add(service(i) == maximum(Array(CPVarInt(cp, twStart(i)), departure(prev(i)) + dist(prev(i))(CPVarInt(cp,i)))))
			
			// A vehicle must finish before end of the time-window
			cp.add(service(i) <= twEnd(i))
			
			// A vehicle must wait for start of the time-window before starting the service
			cp.add(service(i) >= twStart(i))
		}
		
		// A vehicle starts immediately after service
		for(i <- Customers)
			cp.add(departure(i) == service(i) + servDur(i))
			
		// All vehicle start on the morning 
		for(i <- Depots)
			cp.add(departure(i) == 0)
		
	} exploration {
		
		cp.binaryFirstFail(prev)
	}
}