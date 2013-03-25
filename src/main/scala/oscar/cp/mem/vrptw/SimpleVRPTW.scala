package oscar.cp.mem.vrptw
/*
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.RoutingUtils.regretHeuristic
import oscar.cp.mem.vrptw.VRPTWParser.parse
import oscar.cp.constraints.MinAssignment
import scala.collection.mutable.Queue
import oscar.cp.mem.TimeWindowPred
import oscar.cp.mem.NoCycle

/**
 * VRPTW
 *
 * @author Renaud Hartert - ren.hartert@gmail.com
 */

object SimpleVRPTW extends App {
  
  // ------------------------------------------------------------------------
  // DATA AND PARSING
  // ------------------------------------------------------------------------

  val instance = parse("data/VRPTW/Solomon/R104.txt")

  // Distance scaling
  val scale = 100

  // Data
  val nCustomers = instance.n
  val nVehicles = instance.k
  val nSites = nCustomers + 2*nVehicles
  val capacity = instance.c

  val Vehicles = 0 until nVehicles
  val Customers = 0 until nCustomers
  val Sites = 0 until nSites
  val FirstDepots = nCustomers until nCustomers + nVehicles
  val LastDepots = nCustomers + nVehicles until nCustomers + 2*nVehicles

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
  val dist = Array.fill(nSites)(Array.fill(nSites)(0))
  val realDist = Array.fill(nSites)(Array.fill(nSites)(0.0))

  for (c1 <- Sites; c2 <- Sites) {
    val i = if (c1 >= nCustomers) 0 else c1 + 1
    val j = if (c2 >= nCustomers) 0 else c2 + 1
    dist(c1)(c2) = (instance.dist(i)(j) * scale).round.toInt
    realDist(c1)(c2) = (instance.dist(i)(j))
  }

  val Horizon = twStart(FirstDepots.min) to twEnd(LastDepots.min)

  // ------------------------------------------------------------------------
  // VARIABLES
  // ------------------------------------------------------------------------

  val cp = CPSolver()

  val pred = Array.fill(nSites)(CPVarInt(cp, Sites)) 

  val vehicle = Array.fill(nSites)(CPVarInt(cp, Vehicles))
  val arrival = Array.fill(nSites)(CPVarInt(cp, Horizon))
  val load = Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))
  val totDist = CPVarInt(cp, 0 to dist.flatten.sum)

  // ------------------------------------------------------------------------
  // VISUALIZATION
  // ------------------------------------------------------------------------

  val visu = new VisualRelax(coord, realDist)

  // ------------------------------------------------------------------------
  // CONSTRAINTS BLOCK
  // ------------------------------------------------------------------------
  
  cp.minimize(totDist) subjectTo {

    // No cycles
    cp.add(NoCycle(pred, Customers.toSet union LastDepots.toSet))

    // Vehicle
    for (i <- Customers) 
      cp.add(vehicle(pred(i)) == vehicle(i))
      
    for (i <- Vehicles) {
      cp.post(pred(FirstDepots.min + i) == LastDepots.min + i)
    }

    for (i <- Vehicles) {
      // Sets the first depots of vehicle i
      cp.post(vehicle(FirstDepots.min + i) == i)
      // Sets the last depots of vehicle i
      cp.post(vehicle(LastDepots.min + i) == i)
    }
      
    // Capacity of vehicles
    cp.add(binpacking(vehicle, demand, load))

    // Length of the circuit
    cp.add(sum(Sites)(i => dist(i)(pred(i))) == totDist)
    cp.add(new MinAssignment(pred, dist, totDist))

    // Time 
    for (i <- Customers) {

      cp.add(new TimeWindowPred(cp, i, pred, arrival, dist, servDur))
      cp.add(arrival(i) >= arrival(pred(i)) + servDur(pred(i)) + dist(i)(pred(i)))
      cp.add(arrival(i) <= twEnd(i))
      cp.add(arrival(i) >= twStart(i))
    }

    for (i <- FirstDepots) 
      cp.add(arrival(i) == 0)
  }
  
  // Solution
  cp.addDecisionVariables(pred)
  cp.addDecisionVariables(vehicle)
  
  // LNS Relaxation
  // --------------
  
  def clusterRelax(p: Int): Array[Boolean] = {
    val selected = Array.fill(nSites)(false)
    val c = cp.random.nextInt(nCustomers)
    val sortedByDist = Customers.sortBy(i => dist(c)(i))
    val d = dist(c)(sortedByDist(p))
    for (i <- Customers) selected(i) = dist(c)(i) <= d
    selected
  }
  
  def relaxVariables(selected: Array[Boolean]) {

    visu.updateSelected(selected)
    val constraints: Queue[Constraint] = Queue()

    for (i <- Sites; if !selected(i)) {     
      constraints enqueue (vehicle(i) == cp.lastSolution.get(vehicle(i)))    
      if (!selected(cp.lastSolution.get(pred(i)))) {
        constraints enqueue (pred(i) == cp.lastSolution.get(pred(i)))
      }
    }
    cp.post(constraints.toArray)
  }

  var newSol = false
  var p = 5
  val a = 250
  var lastIter = 0
  var iter = 0
  
  cp.exploration {
    regretHeuristic(cp, pred, dist)
    visu.updateRoute(pred.map(_.value))
    visu.updateDist()
    newSol = true
  } 
  
  println("Search...")
  cp.run(1)
  
  val t0 = System.currentTimeMillis()
  while (System.currentTimeMillis() - t0 < 300000) {
    iter += 1
    newSol = false 
    cp.runSubjectTo() { relaxVariables(clusterRelax(p)) }    
    if (newSol) lastIter = iter
    else if (iter - lastIter == a) {
      p += 1
      lastIter = iter
    }
  }
 
  println("\nFinished !")
  cp.printStats
}*/