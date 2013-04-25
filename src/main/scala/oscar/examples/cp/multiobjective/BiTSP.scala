package oscar.examples.cp.multiobjective

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.constraints.ChannelingPredSucc
import oscar.cp.multiobjective.Pareto
import scala.collection.mutable.Queue
import oscar.cp.constraints.MinAssignment
import oscar.cp.multiobjective.ListPareto
import oscar.visual.PlotPareto
import oscar.util.reader.TSPUtils

object BiTSP extends App {
  
  // Parsing
  val nObjs = 2
  val Objs = 0 until nObjs
  val coord1 = TSPUtils.parseCoordinates("data/TSP/renA10.tsp")
  val coord2 = TSPUtils.parseCoordinates("data/TSP/renB10.tsp")
  val distMatrix1 = TSPUtils.buildDistMatrix(coord1)
  val distMatrix2 = TSPUtils.buildDistMatrix(coord2)
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities
  
  val visu = new PlotPareto()
  
  // Model
  // -----
  val cp = new CPSolver()
  cp.silent = true
  
  // Successors & Predecessors
  val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
  val pred = Array.fill(nCities)(CPVarInt(cp, Cities))

  // Total distance
  val totDists = Array.tabulate(nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.paretoMinimize(totDists:_*) subjectTo {

    // Channeling between predecessors and successors
    cp.add(ChannelingPredSucc(pred, succ))

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    for (o <- Objs) {
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
    TSPUtils.regretHeuristic(cp, succ, distMatrices(objective))
  } 
  
  // Run
  // ---  
  println("Search...")
  cp.run()  
 
  cp.printStats() 
  println("Pareto Set")
  println(cp.nonDominatedSolutionsObjs.mkString("\n"))
}