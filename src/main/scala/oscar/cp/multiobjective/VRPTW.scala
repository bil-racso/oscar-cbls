package oscar.cp.multiobjective

import scala.Math.max

import oscar.cp.modeling._
import oscar.cp.core._

/** VRPTW
 * 
 * 	@author Renaud Hartert - ren.hartert@gmail.com
 */

object VRPTW extends App {
	
	val instance = VRPTWParser.parse("")
	
	// Data
	val nCustomers : Int = instance.n	// The number of customers
	val nVehicles  : Int = instance.k	// The number of vehicles
	val capacity   : Int = instance.c	// Capacity of a vehicle
	
	val Vehicles  = 0 until nVehicles
	val Customers = 0 until nCustomers
	val Sites     = 0 until nCustomers + nVehicles
	val Depots    = nCustomers until nCustomers + nVehicles
	
	val demand  : Array[Int] = instance.demand			// Demand of each customer
	val twStart : Array[Int] = instance.twStart			// Earliest delivery time of each customer
	val twEnd   : Array[Int] = instance.twEnd			// Latest delivery time of each customer
	val servDur : Array[Int] = instance.servDur			// Duration needed to serve each customer
	val dist    : Array[Array[Int]] = instance.dist		// Distance matrix between sites
	
	val Horizon = twStart(Depots.min) to twEnd(Depots.min)
	
	// Model
	val cp = CPSolver()
	
	val prev      : Array[CPVarInt] = for(_ <- Sites) yield CPVarInt(cp, Sites)		// Previously visited site
	val next      : Array[CPVarInt] = for(_ <- Sites) yield CPVarInt(cp, Sites)		// Next site to visit
	val routeOf   : Array[CPVarInt] = for(_ <- Sites) yield CPVarInt(cp, Vehicles)	// Route of each vehicle
	val service   : Array[CPVarInt] = for(_ <- Sites) yield CPVarInt(cp, Horizon)	// Date of service of each site
	val departure : Array[CPVarInt] = for(_ <- Sites) yield CPVarInt(cp, Horizon)	// Departure from each site
	
	val totDist = CPVarInt(cp, 0 to dist.flatten.sum)
	
	cp.minimize(totDist) subjectTo {
		
		// Link between prev and next
		for(i <- Sites) {
			cp.add(prev(next(i)) == i)
			cp.add(next(prev(i)) == i)
		}
		
		// TSP constraint
		cp.add(circuit(next), Strong)
		
		// Route consistency
		for(i <- Customers) 
			cp.add(routeOf(i) == routeOf(prev(i)))
			
		// All vehicle start in different depots
		for(i <- Depots)
			cp.add(routeOf(i) == i - Customers.max)
			
		// Capacity of vehicles
		//cp.post(multiknapsack(routeOf, demand, all(k in 1..K) Q));
			
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
		
	} exploration {}
}