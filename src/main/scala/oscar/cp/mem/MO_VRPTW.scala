package oscar.cp.mem

import scala.util.Random.nextFloat
import scala.util.Random.nextInt
import scala.collection.mutable.Queue
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
import oscar.visual.VisualPareto
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.constraints.TONOTCOMMIT

/**
 * VRPTW
 *
 * 	@author Renaud Hartert - ren.hartert@gmail.com
 */

object MO_VRPTW extends App {

  val instance = parse("data/VRPTW/Solomon/R101.txt")

  // Distance scaling
  val scale = 1000

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
  // MOLNS BLOCK
  // ------------------------------------------------------------------------

  case class Sol(pred: Array[Int], succ: Array[Int], route: Array[Int], dist: Int)

  val nObjs = 2
  val Objs = 0 until nObjs
  val pareto = ParetoSet[Sol](nObjs)

  var bestPrev: Array[Int] = new Array(nSites)
  var bestNext: Array[Int] = new Array(nSites)
  var bestRoute: Array[Int] = new Array(nSites)
  var bestDep: Array[Int] = new Array(nSites)
  var bestDist = 0
  var nRestart = 1
  var nObjRestart = 0

  var firstLns = true

  cp.sc = new IDSSearchController(cp, 6)
  var regretSearch = true
  var adaptable = false

  val selected = new Array[Boolean](nSites)


  cp.lns(100, 1000) {

    nRestart += 1

    if (firstLns) {
      println("Start LNS")
      firstLns = false
    }

    if (adaptable) adaptFailure()

    // Next objective 
    val obj = nextObj

    // Relaxation of the objectives (30% Intensification)
    objRelax(obj, nextFloat < 0.3)
      
    // Init data
    bestPrev = pareto.currentSol.pred
    bestNext = pareto.currentSol.succ
    bestRoute = pareto.currentSol.route
    bestDist = pareto.currentSol.dist

    // Variables relaxation
    val nextRelax = if (nextFloat < 0.3) 0 else 1

    relaxVariables(nextRelax match {
      // Customer-based Adaptive Temporal Decomposition
      case 0 => catd((0.15 * nCustomers).toInt)
      // Customer-based Adaptive Spatial Decomposition
      case 1 => casd((0.25 * nCustomers).toInt)
      // Customer-based Adaptive Random Decomposition
      case 2 => card((0.15 * nCustomers).toInt)
      // Relatedness Shaw relaxation
      case 3 => shaw((0.15 * nCustomers).toInt, 10)
    })
  }

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

    val v = if (bestPrev(i) == bestPrev(j)) 0 else 1
    1 / (nDist(i)(j) + v)
  }

  def adaptedAngle(angle: Int, alpha: Int) = {

    val newAngle = angle - alpha
    if (newAngle < 0) 360 - (alpha - angle) else newAngle
  }

  def nextObj: Int = {

    nObjRestart += 1

    val obj = (cp.objective.currentObjectiveIdx + 1) % nObjs

    if (nObjRestart > nObjs) {
      pareto.nextPoint
      nObjRestart = 0
    }

    cp.objective.currentObjective = obj
    obj
  }

  def solFound {
    
    visu.updateRoute(bestPrev)
    visu.updateDist()

    val objs = Array(totDist.value, totTard.value)
    val sol = new Sol(buildPred, buildSucc, buildRoute, buildDist)

    nObjRestart = 0

    // This could be false (framework)
    pareto insert (objs, sol)
  }

  def buildPred: Array[Int] = pred.map(_.value)
  def buildSucc: Array[Int] = succ.map(_.value)
  def buildRoute: Array[Int] = vehicle.map(_.value)
  def buildDist: Int = totDist.value

  // ------------------------------------------------------------------------
  // OBJECTIVES RELAXATION
  // ------------------------------------------------------------------------

  def objRelax(obj: Int, intensification: Boolean = false) {

    for (o <- Objs) {
      if (intensification || o == obj) {
        cp.objective.bounds(o) = pareto.currentPoint(o)
      } else {
        // The -1 avoid to find an already found solution
        cp.objective.bounds(o) = pareto.currentPoint.upperValue(o) - 1
      }
    }

    // Better solution
    cp.objective.bounds(obj) -= 1
  }

  // ------------------------------------------------------------------------
  // VARIABLES PROCEDURES
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
      routes(bestRoute(sortedCustomersByAngle(c))) = true
    }

    for (i <- Customers)
      if (routes(bestRoute(i)))
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
        routes(bestRoute(c)) = true
        nSelected += 1
      }

      i += 1
    }

    for (i <- Customers)
      if (routes(bestRoute(i)))
        selected(i) = true

    selected
  }

  def card(p: Int): Array[Boolean] = {

    val routes = Array.fill(nVehicles)(false)
    val selected = Array.fill(nSites)(false)

    var nSelected = 0

    while (nSelected < p) {

      // Not selected customers
      val remainingC = Customers.filter(!selected(_))
      // Random selection of a not already selected customers
      val alpha = remainingC(nextInt(remainingC.size))

      for (i <- remainingC) {

        val beta = nextInt(remainingC.size)

        if (bestRoute(i) == bestRoute(alpha) && bestDep(i) > bestDep(alpha) && bestDep(i) <= bestDep(beta)) {
          selected(i) = true
          routes(bestRoute(i)) = true
          nSelected += 1
        }
      }
    }

    for (i <- Customers)
      if (routes(bestRoute(i)))
        selected(i) = true

    selected
  }

  def relaxVariables(selected: Array[Boolean]) {

    visu.updateSelected(selected)
    visu.updateDist()
    visu.updateRestart(nRestart)

    val constraints: Queue[Constraint] = Queue()

    for (i <- Sites) {
      if (!selected(i)) {

        constraints enqueue (vehicle(i) == bestRoute(i))

        if (!selected(bestPrev(i)))
          constraints enqueue (pred(i) == bestPrev(i))
        if (!selected(bestNext(i)))
          constraints enqueue (succ(i) == bestNext(i))
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
    for (i <- Customers) {
      cp.add(succ(pred(i)) == i)
      cp.add(pred(succ(i)) == i)
    }

    for (i <- FirstDepots) {
      cp.add(pred(succ(i)) == i)
      cp.add(pred(i) == i + nVehicles)
    }

    for (i <- LastDepots) {
      cp.add(succ(pred(i)) == i)
      cp.add(succ(i) == i - nVehicles)
    }

    // Vehicle
    for (i <- Customers) {
      cp.add(vehicle(i) == vehicle(pred(i)))
      cp.add(vehicle(i) == vehicle(succ(i)))
    }

    for (i <- FirstDepots) {
      cp.add(vehicle(i) == vehicle(succ(i)))
    }

    for (i <- LastDepots) {
      cp.add(vehicle(i) == vehicle(pred(i)))
    }

    for (i <- 0 until nVehicles - 1) {
      cp.add(vehicle(FirstDepots.min + i) == i)
      cp.add(vehicle(LastDepots.min + 1 + i) == i)
    }

    cp.add(vehicle(FirstDepots.max) == Vehicles.max)
    cp.add(vehicle(LastDepots.min) == Vehicles.max)

    // Capacity of vehicles
    cp.add(binpacking(vehicle, demand, load))

    // No cycles
    cp.add(circuit(pred), Strong)
    cp.add(circuit(succ), Strong)

    // Length of the circuit
    cp.add(sum(Sites)(i => dist(i)(pred(i))) == totDist)
    cp.add(sum(Sites)(i => dist(i)(succ(i))) == totDist)

    // Time 
    for (i <- Customers) {

      //cp.add(new TimeWindow(cp, i, pred, succ, arrival, dist, servDur))

      cp.add(arrival(i) >= arrival(pred(i)) + servDur(pred(i)) + dist(i)(pred(i)))
      cp.add(arrival(i) <= arrival(succ(i)) - servDur(i) - dist(i)(succ(i)))

      //cp.add(arrival(i) <= twEnd(i))
      cp.add(arrival(i) >= twStart(i))
    }

    for (i <- FirstDepots) {
      cp.add(arrival(i) <= arrival(succ(i)) - servDur(i) - dist(i)(succ(i)))
      cp.add(arrival(i) == 0)
    }

    for (i <- LastDepots) {
      cp.add(arrival(i) >= arrival(pred(i)) + servDur(pred(i)) + dist(i)(pred(i)))
      cp.add(arrival(i) <= twEnd(i))
    }

    cp.add(new TONOTCOMMIT(cp, pred, dist, totDist))
    cp.add(new TONOTCOMMIT(cp, succ, dist, totDist))
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
    } 
    else {

      while (!allBounds(pred)) {

        val i = selectMin(Sites)(!pred(_).isBound)(pred(_).size).get
        val j = selectMin(Sites)(pred(i).hasValue(_))(dist(i)(_)).get

        cp.branch(cp.post(pred(i) == j))(cp.post(pred(i) != j))
      }
    }

    solFound
  }

  println("\nFinished !")
  cp.printStats
}