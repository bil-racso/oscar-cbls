package oscar.cp.mem.tsp

import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.Pareto
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.pareto.RBPareto
import oscar.cp.mem.pareto.MOSol
import oscar.cp.mem.visu.VisualPareto
import oscar.util._
import scala.collection.mutable.Queue
import oscar.cp.mem.DynDominanceConstraint
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.Gavanelli02

object ExactTriTSP extends App {

  case class Sol(succ: Array[Int])
  
  var pareto = List[MOSol[Sol]]()
  
  val nObjs = 3
  val Objs = 0 until nObjs
  
  // Parsing
  val coord1 = TSPUtils.parseCoordinates("data/TSP/renA10.tsp")
  val coord2 = TSPUtils.parseCoordinates("data/TSP/renB10.tsp")
  val coord3 = TSPUtils.parseCoordinates("data/TSP/renC10.tsp")
  val distMatrix1 = TSPUtils.buildDistMatrix(coord1)
  val distMatrix2 = TSPUtils.buildDistMatrix(coord2)
  val distMatrix3 = TSPUtils.buildDistMatrix(coord3)
  val distMatrices = Array(distMatrix1, distMatrix2, distMatrix3)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities
  
  // Model
  // -----
  val cp = new CPSolver()
  cp.silent = true

  // Successors & Predecessors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  //val pred = Array.fill(nCities)(CPVarInt(cp, Cities))

  // Total distance
  val totDists = Array.tabulate(nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.solve() subjectTo {
    cp.add(circuit(succ))
    for (o <- Objs) cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) == totDists(o))
  }
  
  // Search
  // ------
  val objective = 1
  cp.exploration {
    regretHeuristic(cp, succ, distMatrices(objective))
    solFound()
  } 

  def solFound() {   
    // No dominated solutions !
    val newSol = MOSol(Sol(succ.map(_.value)), totDists.map(_.value))  
    // Insert Solution
    if (!pareto.exists(_ dominates newSol)) {
      pareto = pareto.filter(!newSol.dominates(_))
      pareto = newSol :: pareto
    }
  }
  
  // Run
  // ---  
  println("Search...")
  cp.run()  
 
  cp.printStats() 
  println("Pareto Set")
  val sorted = pareto.sortBy(_(0))
  println(sorted.mkString("\n"))
}