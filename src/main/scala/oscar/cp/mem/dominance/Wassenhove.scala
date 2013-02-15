package oscar.cp.mem.dominance

import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.RBPareto
import oscar.cp.mem.pareto.MOSol
import oscar.cp.mem.visu.VisualSet
import oscar.cp.mem.DominanceConstraint
import oscar.util._
import scala.collection.mutable.Queue
import oscar.cp.mem.DynDominanceConstraint
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.visu.VisualRelax
import oscar.cp.mem.tsp.TSPUtils
import oscar.cp.mem.pareto.Pareto

object Wassenhove extends App {

  case class Sol(pred: Array[Int], succ: Array[Int])

  // BiObjective Pareto Set 
  val pareto: Pareto[Sol] = RBPareto()
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
  //val visu = new VisualSet(pareto)

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
  cp.minimize(totDists(1)) subjectTo {

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
  }
  
  // Search
  // ------
  val objective = 1
  cp.exploration {
    regretHeuristic(cp, succ, distMatrices(objective))
    solFound()
  } 
  
  var sol: MOSol[Sol] = null
  
  def solFound() {   
    
    sol = MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDists.map(_.value))      
    
    // Visu
    //visu.selected(totDists(0).value, totDists(1).value)
    //visu.update()
    //visu.paint
  }
    
  // Run
  // ---  
  println("Search...")
  cp.run()
  do {   
    assert(pareto.insert(sol) != -1)
    cp.objective.objs(0).best = Int.MaxValue  
    cp.runSubjectTo() {
      cp.post(totDists(0) < sol.objs(0))
      sol = null
    }
  } while(sol != null)
  
  cp.printStats()
  println("Pareto Set")
  //println("H: " + oscar.cp.mem.measures.Hypervolume.hypervolume(pareto))
  println(pareto.sortByObj(0).mkString("\n"))
}