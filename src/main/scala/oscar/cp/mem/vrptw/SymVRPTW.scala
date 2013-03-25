package oscar.cp.mem.vrptw

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.constraints.ChannelingPredSucc
import oscar.cp.mem.RoutingUtils.regretHeuristic
import oscar.cp.mem.vrptw.VRPTWParser.parse
import oscar.cp.mem.constraints.TimeWindowPred
import oscar.cp.mem.constraints.TimeWindowSucc
import scala.collection.mutable.Queue
import oscar.util.selectMin

/**
 * VRPTW
 *
 * @author Renaud Hartert - ren.hartert@gmail.com
 */

object SymVRPTW extends App {

  val instance = parse("data/VRPTW/Solomon/C101.txt", 2)

  val nCustomers = instance.nCustomers
  val nVehicles = instance.nVehicles
  val nSites = nCustomers + 2*nVehicles
  val capacity = instance.capacity

  val Vehicles = 0 until nVehicles
  val Customers = 0 until nCustomers
  val Sites = 0 until nCustomers + 2*nVehicles
  val FirstDepots = nCustomers until nCustomers + nVehicles
  val LastDepots = nCustomers + nVehicles until nCustomers + 2*nVehicles

  val demand = instance.demand 
  val twStart = instance.twStart
  val twEnd = instance.twEnd
  val servDur = instance.servDur
  val coord = instance.coord

  val realDist: Array[Array[Double]] = instance.dist
  val dist: Array[Array[Int]] = realDist.map(_.map(_.toInt))

  val Horizon = twStart(FirstDepots.min) to twEnd(LastDepots.min)

  // Variables
  // ---------

  val cp = CPSolver()

  val succ = Array.fill(nSites)(CPVarInt(cp, Sites)) 
  val pred = Array.fill(nSites)(CPVarInt(cp, Sites))  
  
  val load = Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))
  
  val service = Array.fill(nSites)(CPVarInt(cp, Horizon))
  
  val vehicle = Array.fill(nSites)(CPVarInt(cp, Vehicles))
  
  val totDist = CPVarInt(cp, 0 to dist.flatten.sum)

  // Visualization
  // -------------

  val visu = new VisualRelax(coord, realDist)

  // Constraints
  // -----------
  
  cp.minimize(totDist) subjectTo {
    
    cp.add(allDifferent(succ), Strong) 
    cp.add(allDifferent(pred), Strong) 
    
    for (i <- Sites) {
      cp.post(pred(i) != i)
      cp.post(succ(i) != i)
    }
    
    for (i <- Customers) {
      cp.post(pred(succ(i)) == i)
      cp.post(succ(pred(i)) == i)
    }
    
    for (i <- LastDepots) {
      cp.post(succ(pred(i)) == i)
    }
    
    for (i <- FirstDepots) {
      cp.post(pred(succ(i)) == i)
    }
    
    for (i <- Vehicles) {
      cp.post(vehicle(FirstDepots.min + i) == i)
      cp.post(vehicle(LastDepots.min + i) == i)
    }
    
    for (i <- Vehicles) {
      cp.post(succ(LastDepots.min + i) == FirstDepots.min + i)
      cp.post(pred(FirstDepots.min + i) == LastDepots.min + i)
    }
        
    for (i <- Customers) {
      cp.post(vehicle(i) == vehicle(succ(i)))
      cp.post(vehicle(i) == vehicle(pred(i)))
    }
    
    for (i <- LastDepots) {
      cp.post(vehicle(i) == vehicle(pred(i)))
    }
        
    for (i <- FirstDepots) {
      vehicle(i) == vehicle(succ(i))
    }
    
    cp.add(sum(Sites)(i => dist(i)(pred(i))) == totDist)
    cp.add(sum(Sites)(i => dist(i)(succ(i))) == totDist)
  }

  // Solution
  cp.addDecisionVariables(succ)
  cp.addDecisionVariables(vehicle)
  
  cp.exploration {  
    //regretHeuristic(cp, pred, dist) 
    cp.binaryFirstFail(pred)
    visu.updateRoute(succ.map(_.value))
    visu.updateDist()
  } 
  
  println("Search...")
  cp.run(1)

  println("\nFinished !")
  println("Routes")
  val solSucc = Array.tabulate(nSites)(i => cp.lastSol(succ(i)))
  
  for (v <- Vehicles) {
    println("vehicle " + v)
    val first = FirstDepots.min + v
    var i = solSucc(first)
    print(first + " -> " + i)
    while (i != first) {
      print(" -> " + solSucc(i))
      i = solSucc(i)
    }
    println()
  }
}