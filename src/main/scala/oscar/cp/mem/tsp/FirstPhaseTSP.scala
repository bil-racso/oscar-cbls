package oscar.cp.mem.tsp

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
import java.io._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.NewPareto
import oscar.cp.mem.pareto.MOSol

object FirstPhaseTSP {

  case class Sol(pred: Array[Int], succ: Array[Int])

  def main(args: Array[String]) {

    val inst1 = 'A'
    val inst2 = 'B'
    val idTemp = "10c_"

    val pareto: NewPareto[Sol] = NewPareto(2)

    val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/kro" + inst1 + "100.tsp")
    val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/kro" + inst2 + "100.tsp")

    for (i <- 1 to 5) {
      
      val id = idTemp + i

      for (alpha <- 0 to 100) {
        val x = search(alpha, distMatrix1, distMatrix2)
        pareto.insert(x)
        println(x.objs.mkString(" "))
      }

      TSPUtils.writeSet("firstPhase" + inst1 + inst2 + id + ".txt", pareto.map(_.sol.pred).toArray)
      val out = OutFile("firstPhasePoint" + inst1 + inst2 + id + ".txt")
      pareto.foreach(x => out.writeln(x.objs.mkString(" ")))
      out.close()
    }
  }

  def search(alpha: Int, distMatrix1: Array[Array[Int]], distMatrix2: Array[Array[Int]]): MOSol[Sol] = {

    val nCities = distMatrix1.size
    val Cities = 0 until nCities

    val dist = Array.tabulate(nCities)(i => {
      Array.tabulate(nCities)(j => {
        (alpha * distMatrix1(i)(j) + (100 - alpha) * distMatrix2(i)(j)) / 100
      })
    })

    // MODEL
    // --------------------
    val cp = new CPSolver()
    cp.silent = true

    // Successors
    val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
    // Predecessors
    val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
    // Total distance
    val totDist = CPVarInt(cp, 0 to dist.flatten.sum)

    // LNS
    // --------------------
    var currentSol: Sol = null
    val cycleBreaker = true
    val p = 10

    cp.lns(50, 3000) {
      relaxVariables(clusterRelax(p))
    }

    def clusterRelax(p: Int): Array[Boolean] = {

      val c = nextInt(nCities)
      val sortedByDist = Cities.sortBy(i => dist(c)(i))
      val threshold = dist(c)(sortedByDist(p))

      Array.tabulate(nCities)(i => dist(c)(i) <= threshold)
    }

    def solFound() = {
      currentSol = new Sol(pred.map(_.value), succ.map(_.value))
    }

    def relaxVariables(selected: Array[Boolean]) {

      val constraints: Queue[Constraint] = Queue()

      for (c <- Cities; if !selected(c)) {

        val p = currentSol.pred(c)
        val s = currentSol.succ(c)

        if (cycleBreaker) {
          if (!selected(p) && !selected(s)) {
            constraints.enqueue(new InSet(cp, pred(c), Set(p, s)))
            constraints.enqueue(new InSet(cp, succ(c), Set(p, s)))
          }
        } else {
          if (!selected(p)) constraints enqueue (pred(c) == p)
          if (!selected(s)) constraints enqueue (succ(c) == s)
        }

        cp.post(constraints.toArray)
      }
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

    val dist1 = TSPUtils.computeDist(currentSol.pred, distMatrix1)
    val dist2 = TSPUtils.computeDist(currentSol.pred, distMatrix2)
    return MOSol(Sol(currentSol.pred, currentSol.succ), dist1, dist2)
  }
}