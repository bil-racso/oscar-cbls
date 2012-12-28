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
import oscar.cp.mem.pareto.ParetoMinSet
import java.io._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.NewPareto
import oscar.cp.mem.pareto.MOSol

object MolnsTSP {

  case class Sol(pred: Array[Int], succ: Array[Int])
  
  def main(args: Array[String]) {
    
    val pareto: NewPareto[Sol] = NewPareto(2)
    
    val preds = TSPUtils.readSet("firstPhase.txt")
    val succs = TSPUtils.buildSuccsFromPreds(preds)
    
    val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/kroA100.tsp")
    val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/kroB100.tsp")
    
    for (i <- 0 until preds.size) {
      val dist1 = TSPUtils.computeDist(preds(i), distMatrix1)
      val dist2 = TSPUtils.computeDist(preds(i), distMatrix2)
      val x = MOSol(Sol(preds(i), succs(i)), dist1, dist2)
      pareto insert x
    }
    
    solveMO(pareto, distMatrix1, distMatrix2)

    TSPUtils.writeSet("firstPhase.txt", pareto.map(_.sol.pred).toArray)
  }

  def solveMO(pareto: NewPareto[Sol], distMatrix1: Array[Array[Int]], distMatrix2: Array[Array[Int]]) = {

    val nCities = distMatrix1.size
    val Cities = 0 until nCities

    // Model
    // -----
    val cp = new CPSolver()
    cp.silent = true

    // Successors & Predecessors
    val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
    val pred = Array.fill(nCities)(CPVarInt(cp, Cities))

    // Total distance
    val totDist1 = CPVarInt(cp, 0 to distMatrix1.flatten.sum)
    val totDist2 = CPVarInt(cp, 0 to distMatrix2.flatten.sum)

    // MOLNS
    // -----
    var newSol: MOSol[Sol] = null
    var currentSol: MOSol[Sol] = null
    var currentObjective = 0
    val p = 15

    cp.lns(5000, 2000) {  
      if (!insertNewSolution() && currentObjective < pareto.Objs.max) currentObjective += 1
      else {
        currentObjective = 0
        selectSolution()        
      }
      relaxObjectives(currentObjective)
      relaxVariables(clusterRelax(p))
    }

    def insertNewSolution(): Boolean = {
      if (newSol == null) false
      else {
        val removed = newSol dominates currentSol
        pareto insert newSol
        newSol = null
        removed
      }
    }

    def selectSolution() { 
      val r = nextInt(pareto.size)
      currentSol = pareto(r)    
    }
    
    def relaxObjectives(obj: Int, intensification: Boolean = false) {
      for (o <- pareto.Objs) {
        if (intensification || o == obj) {
          cp.objective.objs(o).best = currentSol.objs(o)
          cp.objective.objs(o).tightenMode = TightenType.StrongTighten
        }
        else {
          cp.objective.objs(o).best = pareto.upper(currentSol.objs(o), o) - 1
          cp.objective.objs(o).tightenMode = TightenType.WeakTighten
        }
      }
    }

    def clusterRelax(p: Int): Array[Boolean] = {
      
      val distMatrix = if (currentObjective == 0) distMatrix1 else distMatrix2
      
      val c = nextInt(nCities)
      val sortedByDist = Cities.sortBy(i => distMatrix(c)(i))
      val dist = distMatrix(c)(sortedByDist(p))

      Array.tabulate(nCities)(i => distMatrix(c)(i) <= dist)
    }

    def solFound() {
      newSol = MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDist1.value, totDist2.value)
    }

    def relaxVariables(selected: Array[Boolean]) {

      val constraints: Queue[Constraint] = Queue()

      for (c <- Cities; if !selected(c)) {
        val p = currentSol.sol.pred(c)
        val s = currentSol.sol.succ(c)
        if (!selected(p) && !selected(s)) {
          constraints.enqueue(new InSet(cp, pred(c), Set(p, s)))
          constraints.enqueue(new InSet(cp, succ(c), Set(p, s)))
        }
      }
      cp.post(constraints.toArray)
    }

    // Constraints
    // -----------
    cp.minimize(totDist1, totDist2) subjectTo {

      // Channeling between predecessors and successors
      cp.add(new ChannelingPredSucc(cp, pred, succ))

      // Consistency of the circuit with Strong filtering
      cp.add(circuit(succ), Strong)
      cp.add(circuit(pred), Strong)

      // Total distance 1
      cp.add(sum(Cities)(i => distMatrix1(i)(succ(i))) == totDist1)
      cp.add(sum(Cities)(i => distMatrix1(i)(pred(i))) == totDist1)

      cp.add(new TONOTCOMMIT(cp, pred, distMatrix1, totDist1))
      cp.add(new TONOTCOMMIT(cp, succ, distMatrix1, totDist1))

      // Total distance 2
      cp.add(sum(Cities)(i => distMatrix2(i)(succ(i))) == totDist2)
      cp.add(sum(Cities)(i => distMatrix2(i)(pred(i))) == totDist2)

      cp.add(new TONOTCOMMIT(cp, pred, distMatrix2, totDist2))
      cp.add(new TONOTCOMMIT(cp, succ, distMatrix2, totDist2))
    }

    // Search
    // ------
    println("Searching...")
    cp.exploration {
      val distMatrix = if (currentObjective == 0) distMatrix1 else distMatrix2
      regretHeuristic(cp, succ, distMatrix)
      solFound()
    }
  }
}