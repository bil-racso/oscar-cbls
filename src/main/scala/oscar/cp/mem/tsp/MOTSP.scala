package oscar.cp.mem.tsp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.constraints.TONOTCOMMIT
import scala.util.continuations._
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.RoutingUtils._
import oscar.cp.mem.pareto.MOSol

abstract class MOTSP[Sol](selected: Array[Array[Boolean]], distMatrices: Array[Array[Int]]*) {
  
  var currentObjective = 0
  
  def solFound()

  val nCities = distMatrices(0).size
  val Cities = 0 until nCities

  val nObjs = distMatrices.size
  val Objs = 0 until nObjs

  // Model
  // -----
  val cp = new CPSolver()
  cp.silent = true

  // Successors & Predecessors
  val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))
  val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))

  // Total distance
  val totDists = Array.tabulate(nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.minimize(totDists: _*) subjectTo {

    // Channeling between predecessors and successors
    cp.add(new ChannelingPredSucc(cp, pred, succ))

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    for (o <- Objs) {
      cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) == totDists(o))
      cp.add(sum(Cities)(i => distMatrices(o)(i)(pred(i))) == totDists(o))
      cp.add(new TONOTCOMMIT(cp, pred, distMatrices(o), totDists(o)))
      cp.add(new TONOTCOMMIT(cp, succ, distMatrices(o), totDists(o)))
    }
  }

  cp.exploration {
    minDomDistHeuristic(cp, succ, distMatrices(currentObjective))
    solFound()
  }     
}