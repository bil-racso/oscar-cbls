package oscar.cp.mem.vrptw

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.constraints.TONOTCOMMIT
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.RoutingUtils.minDomDistHeuristic
import oscar.cp.mem.vrptw.VRPTWParser.parse
import oscar.cp.mem.TimeWindowPred
import oscar.cp.mem.TimeWindowSucc

/**
 * VRPTW
 *
 * @author Renaud Hartert - ren.hartert@gmail.com
 */

object SymVRPTW extends App {
  
  // ------------------------------------------------------------------------
  // DATA AND PARSING
  // ------------------------------------------------------------------------

  val instance = parse("data/VRPTW/Solomon/R104.txt")

  // Distance scaling
  val scale = 100

  // Data
  val nCustomers = instance.n
  val nVehicles = instance.k
  val nSites = nCustomers + 2 * nVehicles
  val capacity = instance.c

  val Vehicles = 0 until nVehicles
  val Customers = 0 until nCustomers
  val Sites = 0 until nCustomers + 2 * nVehicles
  val FirstDepots = nCustomers until nCustomers + nVehicles
  val LastDepots = nCustomers + nVehicles until nCustomers + 2 * nVehicles

  val demand = new Array[Int](nSites) 
  val twStart = new Array[Int](nSites) 
  val twEnd = new Array[Int](nSites)
  val servDur = new Array[Int](nSites)

  val coord = new Array[(Int, Int)](nSites)

  for (s <- Sites) {
    val i = if (s >= nCustomers) 0 else s + 1

    demand(s) = instance.demand(i)
    twStart(s) = instance.twStart(i) * scale
    twEnd(s) = instance.twEnd(i) * scale
    servDur(s) = instance.servDur(i) * scale

    coord(s) = instance.coord(i)
  }

  // Distance matrix between sites
  val dist = new Array[Array[Int]](nSites, nSites)
  val realDist = new Array[Array[Double]](nSites, nSites)

  for (c1 <- Sites; c2 <- Sites) {
    val i = if (c1 >= nCustomers) 0 else c1 + 1
    val j = if (c2 >= nCustomers) 0 else c2 + 1
    dist(c1)(c2) = (instance.dist(i)(j) * scale).round.toInt
    realDist(c1)(c2) = (instance.dist(i)(j))
  }

  val Horizon = twStart(FirstDepots.min) to twEnd(FirstDepots.min)

  // ------------------------------------------------------------------------
  // VARIABLES
  // ------------------------------------------------------------------------

  val cp = CPSolver()

  val pred = Array.fill(nSites)(CPVarInt(cp, Sites)) 
  val succ = Array.fill(nSites)(CPVarInt(cp, Sites))  
  val vehicle = Array.fill(nSites)(CPVarInt(cp, Vehicles))
  val arrival = Array.fill(nSites)(CPVarInt(cp, Horizon))
  val departure = Array.fill(nSites)(CPVarInt(cp, Horizon))
  val load = Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))
  val totDist = CPVarInt(cp, 0 to dist.flatten.sum)

  case class Sol(pred: Array[Int], succ: Array[Int], vehicle: Array[Int], dist: Int) 
  var currentSol: Sol = null
  
  def solFound() {    
    currentSol = new Sol(pred.map(_.value), succ.map(_.value), vehicle.map(_.value), totDist.value)
    visu.updateRoute(currentSol.pred)
    visu.updateDist()
  }

  // ------------------------------------------------------------------------
  // VISUALIZATION
  // ------------------------------------------------------------------------

  val visu = new VisualRelax(coord, realDist)

  // ------------------------------------------------------------------------
  // CONSTRAINTS BLOCK
  // ------------------------------------------------------------------------
  
  cp.minimize(totDist) subjectTo {

    // Successor and Predecessor
    cp.add(new ChannelingPredSucc(cp, pred, succ))

    for (i <- 1 to Vehicles.max) {
      cp.add(pred(FirstDepots.min + i) == LastDepots.min + i - 1)
      cp.add(succ(LastDepots.min + i - 1) == FirstDepots.min + i)
    }

    cp.add(pred(FirstDepots.min) == LastDepots.max)
    cp.add(succ(LastDepots.max) == FirstDepots.min)

    // No cycles
    cp.add(circuit(pred), Strong)
    cp.add(circuit(succ), Strong)

    // Vehicle
    for (i <- Customers) {
      cp.add(vehicle(succ(i)) == vehicle(i))
      cp.add(vehicle(pred(i)) == vehicle(i))
    }

    for (i <- Vehicles) {
      cp.add(vehicle(FirstDepots.min + i) == vehicle(succ(FirstDepots.min + i)))
      cp.add(vehicle(LastDepots.min + i) == vehicle(pred(LastDepots.min + i)))
      
      cp.add(vehicle(FirstDepots.min + i) == i)
      cp.add(vehicle(LastDepots.min + i) == i)
    }

    // Capacity of vehicles
    cp.add(binpacking(vehicle, demand, load))

    // Length of the circuit
    cp.add(sum(Sites)(i => dist(i)(pred(i))) == totDist)
    cp.add(sum(Sites)(i => dist(i)(succ(i))) == totDist)

    cp.add(new TONOTCOMMIT(cp, pred, dist, totDist))
    cp.add(new TONOTCOMMIT(cp, succ, dist, totDist))

    // Time 
    for (i <- Customers) {

      cp.add(new TimeWindowPred(cp, i, pred, arrival, dist, servDur))
      cp.add(new TimeWindowSucc(cp, i, succ, arrival, dist, servDur))

      cp.add(arrival(i) >= arrival(pred(i)) + servDur(pred(i)) + dist(i)(pred(i)))
      cp.add(arrival(i) <= arrival(succ(i)) - servDur(i) - dist(i)(succ(i)))

      cp.add(arrival(i) <= twEnd(i))
      cp.add(arrival(i) >= twStart(i))
    }

    for (i <- FirstDepots) {      
      cp.add(new TimeWindowSucc(cp, i, succ, arrival, dist, servDur))
      cp.add(arrival(i) == 0)
    }

    for (i <- LastDepots) {
      cp.add(new TimeWindowPred(cp, i, pred, arrival, dist, servDur))
      cp.add(arrival(i) <= twEnd(i))
    }
  }

  // ------------------------------------------------------------------------
  // EXPLORATION BLOCK
  // ------------------------------------------------------------------------

  cp.exploration {
    //regretHeuristic(cp, succ, dist)
    minDomDistHeuristic(cp, pred, succ, dist)
    solFound()
  } 
  
  println("\nFinished !")
  cp.printStats
}