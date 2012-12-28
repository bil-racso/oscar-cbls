package oscar.cp.mem.tsp

import oscar.cp.mem.tsp.TSPParser.parseCoordinates
import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import oscar.cp.constraints._
import oscar.cp.mem.visu.VisualRelax
import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.util.Random.nextFloat
import scala.math.round
import oscar.search.IDSSearchController
import oscar.cp.mem.pareto.ParetoMinSet
import java.io._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet

class FirstPhaseTSP {
  
  case class MOSol(pred: Array[Int], succ: Array[Int], dist1: Int, dist2: Int)
  case class Sol(pred: Array[Int], succ: Array[Int], dist: Int)

  val nObjs = 2
  val Objs = 0 until nObjs
  val pareto = ParetoMinSet[MOSol]()
  
  val used = Array.fill(100)(Array.fill(100)(false))
  
  def solve(alpha: Int, instance1: String, instance2: String): MOSol = {

    // Data parsing
    // ------------
    val coord1 = parseCoordinates(instance1)
    val coord2 = parseCoordinates(instance2)

    val nCities = coord1.size
    val Cities = 0 until nCities

    // Computes the distance between two cities
    def getDist(p1: (Int, Int), p2: (Int, Int)): Double = {
      val dx = p2._1 - p1._1
      val dy = p2._2 - p1._2
      math.sqrt(dx * dx + dy * dy)
    }

    // Builds the distance matrix
    val realDistMatrix1 = Array.tabulate(nCities, nCities)((i, j) => getDist(coord1(i), coord1(j)))
    val realDistMatrix2 = Array.tabulate(nCities, nCities)((i, j) => getDist(coord2(i), coord2(j)))

    val distMatrix1 = realDistMatrix1.map(_.map(math.round(_).toInt))
    val distMatrix2 = realDistMatrix2.map(_.map(math.round(_).toInt))

    val dist = Array.tabulate(nCities)(i => {
      Array.tabulate(nCities)(j => {
        (alpha * distMatrix1(i)(j) + (100 - alpha) * distMatrix2(i)(j)) / 100
      })
    })

    // Model
    // -----
    val cp = new CPSolver()

    // Successors
    val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
    // Predecessors
    val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
    // Total distance
    val totDist = CPVarInt(cp, 0 to dist.flatten.sum)

    // Visualization
    // -------------
    //val visu: VisualRelax = new VisualRelax(coord1, dist.map(_.map(_.toDouble)))

    // LNS
    // ---
    var currentSol: Sol = null

    var nRestart = 1
    var nStagnation = 0
    var stagnation = false

    val pMin = 15
    val pMax = 60
    var p = pMin

    cp.silent = true
    cp.lns(400, 3000) {

      nRestart += 1

      handleStagnation()

      relaxVariables(pathRelax(p))
    }

    def handleStagnation() {

      if (stagnation) nStagnation += 1
      else {
        stagnation = true
        nStagnation = 0
        p = pMin + (p - pMin) / 2
      }

      if (nStagnation == 20) {
        nStagnation = 0
        if (p < pMax) p += 1
      }
    }

    def pathRelax(p: Int): Array[Boolean] = {

      val c = nextInt(nCities)
      val selected = Array.fill(nCities)(false)
      selected(c) = true

      for (i <- 1 until p) {

        val sel = Cities.filter(i => selected(i))
        val rem = Cities.filter(i => !selected(i))

        val c = sel(nextInt(sel.size))
        val cc = rem.sortBy(i => dist(c)(i)).head

        selected(cc) = true
      }
      selected
    }

    def solFound() = {
      stagnation = false
      currentSol = new Sol(pred.map(_.value), succ.map(_.value), totDist.value)
      //visu.updateRoute(currentSol.pred)
      //visu.updateDist()
    }

    def relaxVariables(selected: Array[Boolean]) {

      //visu.updateSelected(selected)
      //visu.updateRestart(nRestart)

      val constraints: Queue[Constraint] = Queue()

      for (c <- Cities; if !selected(c)) {
        if (!selected(currentSol.pred(c)))
          constraints enqueue (pred(c) == currentSol.pred(c))
        if (!selected(currentSol.succ(c)))
          constraints enqueue (succ(c) == currentSol.succ(c))
      }
      cp.post(constraints.toArray)
    }

    // Constraints
    // -----------
    cp.minimize(totDist) subjectTo {

      // Channeling between predecessors and successors
      cp.add(new ChannelingPredSucc(cp, pred, succ))

      // Consistency of the circuit with Strong filtering
      cp.add(circuit(succ), Strong)
      cp.add(circuit(pred), Strong)

      // Total distance
      cp.add(sum(Cities)(i => dist(i)(succ(i))) == totDist)
      cp.add(sum(Cities)(i => dist(i)(pred(i))) == totDist)

      cp.add(new TONOTCOMMIT(cp, pred, dist, totDist))
      cp.add(new TONOTCOMMIT(cp, succ, dist, totDist))
    }

    // Search
    // ------
    cp.exploration {
      regretHeuristic(cp, pred, dist)
      solFound()
    }

    var dist1 = 0
    for (i <- Cities) dist1 += distMatrix1(i)(currentSol.pred(i))

    var dist2 = 0
    for (i <- Cities) dist2 += distMatrix2(i)(currentSol.pred(i))

    return new MOSol(currentSol.pred, currentSol.succ, dist1, dist2)
  }

}