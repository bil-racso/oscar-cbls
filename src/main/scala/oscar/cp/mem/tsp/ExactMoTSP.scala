package oscar.cp.mem.tsp

import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.pareto.MOSol
import oscar.cp.mem.visu.VisualSet
import oscar.cp.mem.DominanceConstraint
import oscar.util._
import scala.collection.mutable.Queue
import oscar.cp.mem.DynDominanceConstraint
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.visu.VisualRelax

object ExactMoTSP extends App {

  case class Sol(pred: Array[Int], succ: Array[Int])

  // BiObjective Pareto Set 
  val pareto: ParetoSet[Sol] = ParetoSet(2)
  pareto.Objs.foreach(pareto.nadir(_) = 10000)
  
  // Parsing
  val coord1 = TSPUtils.parseCoordinates("data/TSP/renA15.tsp")
  val coord2 = TSPUtils.parseCoordinates("data/TSP/renB15.tsp")
  val distMatrix1 = TSPUtils.buildDistMatrix(coord1)
  val distMatrix2 = TSPUtils.buildDistMatrix(coord2)
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities
  
  // Visualization
  val visu = new VisualSet(pareto)
  val visuT1 = new VisualRelax(coord1, TSPUtils.buildRealDistMatrix(coord1))
  val visuT2 = new VisualRelax(coord2, TSPUtils.buildRealDistMatrix(coord2))

  // Model
  // -----
  val cp = new CPSolver()
  cp.silent = true

  // Successors & Predecessors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  val pred = Array.fill(nCities)(CPVarInt(cp, Cities))

  // Total distance
  val totDists = Array.tabulate(pareto.nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.solve() subjectTo {

    // Channeling between predecessors and successors
    cp.add(new ChannelingPredSucc(cp, pred, succ))

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    for (o <- pareto.Objs) {
      cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) == totDists(o))
      cp.add(sum(Cities)(i => distMatrices(o)(i)(pred(i))) == totDists(o))
      cp.add(new MinAssignment(pred, distMatrices(o), totDists(o)))
      cp.add(new MinAssignment(succ, distMatrices(o), totDists(o)))
    }
    
    cp.add(new DynDominanceConstraint(cp, pareto, totDists:_*))
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
    val newSol = MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDists.map(_.value))    
    assert(pareto.insert(newSol) != -1)
    
    // Visu
    visu.selected(totDists(0).value, totDists(1).value)
    visu.update()
    visu.paint
    visuT1.updateRoute(pred.map(_.value))
    visuT2.updateRoute(pred.map(_.value))
  }
  
  // Run
  // ---  
  println("Search...")
  cp.run()  
 
  cp.printStats() 
  println("Pareto Set")
  println("H: " + oscar.cp.mem.measures.Hypervolume.hypervolume(pareto))
  println(pareto.sortedByObj(0).mkString("\n"))
}