package oscar.cp.mem

import scala.collection.mutable.Queue
import scala.collection.mutable.Set
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
import oscar.search.IDSSearchController
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.constraints.TONOTCOMMIT

/**
 * VRPTW
 *
 * @author Renaud Hartert - ren.hartert@gmail.com
 */

object VRPTW extends App {

  val instance = parse("data/VRPTW/Solomon/C101.txt")

  // Distance scaling
  val scale = 1000

  // Data
  val nCustomers = instance.n
  val nVehicles = 10 //instance.k
  val nSites = nCustomers + 2 * nVehicles
  val capacity = instance.c

  val Vehicles = 0 until nVehicles
  val Customers = 0 until nCustomers
  val Sites = 0 until nCustomers + 2 * nVehicles
  val FirstDepots = nCustomers until nCustomers + nVehicles
  val LastDepots = nCustomers + nVehicles until nCustomers + 2 * nVehicles

  val demand = new Array[Int](nSites) // Demand of each customer
  val twStart = new Array[Int](nSites) // Earliest delivery time of each customer
  val twEnd = new Array[Int](nSites) // Latest delivery time of each customer
  val servDur = new Array[Int](nSites) // Duration needed to serve each customer

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
    dist(c1)(c2) = (instance.dist(i)(j) * scale).toInt
    realDist(c1)(c2) = (instance.dist(i)(j))
  }

  val Horizon = twStart(FirstDepots.min) to twEnd(FirstDepots.min)

  // Model
  val cp = CPSolver()

  val pred = Array.fill(nSites)(CPVarInt(cp, Sites)) // Predecessor
  val succ = Array.fill(nSites)(CPVarInt(cp, Sites)) // Successor 
  val vehicle = Array.fill(nSites)(CPVarInt(cp, Vehicles)) // Route of each vehicle
  val arrival = Array.fill(nSites)(CPVarInt(cp, Horizon)) // Date of service of each site

  val load = Array.fill(nVehicles)(CPVarInt(cp, 0 to capacity))

  val totDist = CPVarInt(cp, 0 to dist.flatten.sum)
  val totTard = CPVarInt(cp, Horizon)

  // ------------------------------------------------------------------------
  // LNS BLOCK
  // ------------------------------------------------------------------------

  case class Sol(pred: Array[Int], succ: Array[Int], vehicle: Array[Int], dist: Int)
  
  var currentSol: Sol = null

  var nRestart = 1
  var nStagnation = 0
  var stagnation = false

  val pMin = 15
  val pMax = 35
  var p = pMin

  val tabuRelax = Set[Array[Boolean]]()
  var relaxOk = false

  var firstLns = true

  //cp.sc = new IDSSearchController(cp, 4)
  var adaptable = false
  var regretSearch = false

  cp.failLimit = 5
  /*cp.lns(500, 1000) {

    nRestart += 1

    if (firstLns) {
      println("Start LNS")
      firstLns = false
      regretSearch = true
    }

    if (stagnation) {
      nStagnation += 1
    } else {
      // Reset stagnation
      stagnation = true
      nStagnation = 0
      p = pMin + (p-pMin)/2
      // Reset Tabu relax
      tabuRelax.clear()
    }

    if (nStagnation == 10) {
      nStagnation = 0
      p += 1
      if (p > pMax) p = pMax
      // If p increase, tabuRelax is not relevant
      tabuRelax.clear
    }

    if (adaptable) adaptFailure()

    val nextRelax = 2

    var sel: Array[Boolean] = null
    
    relaxOk = false
    while (!relaxOk) {
      
      sel = nextRelax match {
        // Customer-based Adaptive Temporal Decomposition
        case 0 => catd(p)
        // Customer-based Adaptive Spatial Decomposition
        case 1 => casd(p)
        // Relatedness Shaw relaxation
        case 2 => shaw(p, 15)
      }
      
      if (!tabuRelax.contains(sel)) {
        relaxOk = true
        // Warning ! must be added only if R (not !) in order to consider random exploration 
        tabuRelax add sel
      }
    }
    
    relaxVariables(sel)
  }*/

  // ------------------------------------------------------------------------
  // PREPROCESSING AND USEFUL FUNCTIONS
  // ------------------------------------------------------------------------

  val sortedCustomersByTwStart = Customers.sortBy(i => twStart(i))
  val sortedCustomersByTwEnd = Customers.sortBy(i => twEnd(i))

  val angles = Array.tabulate(nCustomers)(i => {

    val x = coord(i)._1 - coord(FirstDepots.min)._1
    val y = coord(i)._2 - coord(FirstDepots.min)._2

    val theta = (atan2(x, y) * 180 / Pi).toInt
    if (theta < 0) (theta + 360) else theta
  })

  val sortedCustomersByAngle = Customers.sortBy(i => angles(i))

  // Normalized distances
  val maxDist = dist.map(_.max).max
  val nDist = dist.map(_.map(_ / maxDist.toDouble))

  def relatedness(i: Int, j: Int) = {

    val v = if (currentSol.vehicle(i) == currentSol.vehicle(j)) 0 else 1
    1 / (nDist(i)(j) + v)
  }

  def adaptedAngle(angle: Int, alpha: Int) = {

    val newAngle = angle - alpha
    if (newAngle < 0) 360 - (alpha - angle) else newAngle
  }

  def solFound {

    stagnation = false
    
    currentSol = new Sol(pred.map(_.value), succ.map(_.value), vehicle.map(_.value), totDist.value)

    visu.updateRoute(currentSol.pred)
    visu.updateDist()
  }

  // ------------------------------------------------------------------------
  // RELAXATION PROCEDURES
  // ------------------------------------------------------------------------

  def adaptFailure() {

    if (!cp.isLastLNSRestartCompleted)
      cp.failLimit = (cp.failLimit * 110) / 100
    else
      cp.failLimit = max(10, (cp.failLimit * 90) / 100)
  }

  def shaw(p: Int, beta: Int): Array[Boolean] = {

    // Random selection of a customer	
    val selected = Array.fill(nSites)(false)
    selected(nextInt(nCustomers)) = true

    for (k <- 1 until p) {

      val selectedC = Customers.filter(selected(_))
      val remainingC = Customers.filter(!selected(_))

      // Selects randomly a customer in S
      val c = selectedC(nextInt(selectedC.size))

      // Order by relatedness with c
      val sortedC = remainingC.sortWith((i, j) => relatedness(c, i) > relatedness(c, j))

      val r = sortedC((pow(nextFloat, beta) * sortedC.size).floor.toInt)
      selected(r) = true
    }

    selected
  }

  def casd(p: Int): Array[Boolean] = {

    val routes = Array.fill(nVehicles)(false)
    val selected = Array.fill(nSites)(false)

    val alpha = nextInt(360)

    var first = 0
    var min = Int.MaxValue

    for (i <- sortedCustomersByAngle) {
      val angle = if (angles(i) - alpha < 0) 360 - (alpha - angles(i)) else angles(i) - alpha
      if (angle < min) {
        first = i
        min = angle
      }
    }

    for (i <- first until first + p) {
      val c = if (i < nCustomers) i else i - nCustomers
      routes(currentSol.vehicle(sortedCustomersByAngle(c))) = true
    }

    for (i <- Customers)
      if (routes(currentSol.vehicle(i)))
        selected(i) = true

    selected
  }

  def catd(p: Int): Array[Boolean] = {

    val routes = Array.fill(nVehicles)(false)
    val selected = Array.fill(nSites)(false)

    // Ensures the relaxation of p customers (not depots)
    val max = nCustomers - p - 1
    val alpha = nextInt(twStart(sortedCustomersByTwStart(max)))

    var nSelected = 0
    var i = 0

    // Keeps relevant customers
    while (nSelected < p) {

      val c = sortedCustomersByTwEnd(i)

      if (twStart(c) >= alpha) {
        routes(currentSol.vehicle(c)) = true
        nSelected += 1
      }

      i += 1
    }

    for (i <- Customers)
      if (routes(currentSol.vehicle(i)))
        selected(i) = true

    selected
  }

  def relaxVariables(selected: Array[Boolean]) {

    visu.updateSelected(selected)
    visu.updateRestart(nRestart)

    val constraints: Queue[Constraint] = Queue()

    for (i <- Sites) {
      if (!selected(i)) {

        constraints enqueue (vehicle(i) == currentSol.vehicle(i))

        if (!selected(currentSol.pred(i))) {
          constraints enqueue (pred(i) == currentSol.pred(i))
        }
        if (!selected(currentSol.succ(i)))
          constraints enqueue (succ(i) == currentSol.succ(i))
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

    // Successor and Predecessor
    for (i <- Sites) {
      cp.add(element(succ, pred(i)) == i)
      cp.add(element(pred, succ(i)) == i)
    }

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
      cp.add(vehicle(i) == vehicle(pred(i)))
      cp.add(vehicle(i) == vehicle(succ(i)))
    }

    for (i <- Vehicles) {
      cp.add(vehicle(FirstDepots.min + i) == vehicle(succ(FirstDepots.min + i)))
      cp.add(vehicle(LastDepots.min + i) == vehicle(pred(LastDepots.min + i)))
    }

    for (i <- Vehicles) {
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

    for (i <- FirstDepots) cp.add(arrival(i) == 0)

    for (i <- LastDepots) cp.add(arrival(i) <= twEnd(i))
  }

  // ------------------------------------------------------------------------
  // EXPLORATION BLOCK
  // ------------------------------------------------------------------------

  cp.exploration {

    if (regretSearch) {

      while (!allBounds(succ)) {

        var x = -1
        var maxRegret = Int.MinValue

        for (i <- Sites; if (!succ(i).isBound)) {

          var distK1 = Int.MaxValue
          var distK2 = Int.MaxValue

          for (j <- Sites; if (succ(i).hasValue(j))) {

            if (dist(i)(j) < distK1) {
              distK2 = distK1
              distK1 = dist(i)(j)
            } else if (dist(i)(j) < distK2) {
              distK2 = dist(i)(j)
            }
          }

          val regret = distK2 - distK1

          if (regret > maxRegret) {
            x = i
            maxRegret = regret
          }
        }

        val v = selectMin(Sites)(succ(x).hasValue(_))(dist(x)(_)).get

        cp.branch(cp.post(succ(x) == v))(cp.post(succ(x) != v))
      }
    } else {

      while (!allBounds(succ)) {
        
        println(pred.map(_.size).sum + succ.map(_.size).sum + vehicle.map(_.size).sum)

        val i = selectMin(Sites)(!succ(_).isBound)(succ(_).size).get
        val j = selectMin(Sites)(succ(i).hasValue(_))(dist(i)(_)).get

        cp.branch(cp.post(succ(i) == j))(cp.post(succ(i) != j))
      }
    }

    solFound
  }
  

  println("\nFinished !")
  cp.printStats
}