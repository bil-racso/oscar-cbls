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
import oscar.cp.mem.visu.PlotPareto
import oscar.util._
import scala.collection.mutable.Queue
import oscar.cp.mem.DynDominanceConstraint
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.Gavanelli02
import oscar.cp.mem.pareto.ListPareto

object ExactTriTSP extends App {

  case class Sol(succ: Array[Int])
  val pareto = new ListPareto[Sol](3)
  
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
  val cp = CPSolver()
  cp.silent = true

  // Successors & Predecessors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))

  // Total distance
  val totDists = Array.tabulate(pareto.nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.solve() subjectTo {
    cp.add(circuit(succ))
    for (o <- pareto.Objs) cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) == totDists(o))   
    cp.add(Gavanelli02(pareto, totDists:_*))
  }
  
  // Search
  // ------
  val objective = 0
  cp.exploration {
    regretHeuristic(cp, succ, distMatrices(objective))
    solFound()
  } 

  def solFound() {   
    // No dominated solutions !
    val newSol = MOSol(Sol(succ.map(_.value)), totDists.map(_.value))      
    // Insert Solution
    assert(pareto.insert(newSol) != -1)
  }
  
  // Run
  // ---  
  cp.run()  
  
  val sols = pareto.toList
  
  for (sol1 <- sols; sol2 <- sols; if sol1 != sol2) {
    assert(!sol1.dominates(sol2))
    assert(!sol2.dominates(sol1))
  }
 
  println("Pareto Set")
  println(sols.sortBy(_.objVals(0)).mkString("\n"))
}