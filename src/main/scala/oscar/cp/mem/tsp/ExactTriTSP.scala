package oscar.cp.mem.tsp

import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.Pareto
import oscar.cp.mem.visu.PlotPareto
import oscar.util._
import scala.collection.mutable.Queue
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.Gavanelli02
import oscar.cp.mem.pareto.ListPareto
//import oscar.cp.mem.dominance.SimpleQuadTree

object ExactTriTSP extends App {

  case class Sol(succ: Array[Int])
  val pareto = ListPareto[Sol](3, maximization = false)
  
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

  // Total distance
  val totDists = Array.tabulate(pareto.nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.solve() subjectTo {
    cp.add(circuit(succ))
    for (o <- pareto.Objs) cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) == totDists(o))   
    cp.add(Gavanelli02(pareto, Array.fill(pareto.nObjs)(false), totDists))
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
    val newSol = Sol(succ.map(_.value))      
    // Insert Solution
    val inserted = pareto.insert(newSol, totDists.map(_.value):_*)
    assert(inserted)
  }
  
  // Run
  // ---  
  cp.run()  
  
  val sols = pareto.objectiveSols
 
  println("Pareto Set")
  println(sols.sortBy(_(0)).mkString("\n"))
}