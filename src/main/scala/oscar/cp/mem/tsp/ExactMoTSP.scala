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
import oscar.cp.constraints.TONOTCOMMIT
import oscar.util._

object ExactMoTSP extends App {

  case class Sol(pred: Array[Int], succ: Array[Int])

  // Pareto set
  val pareto: ParetoSet[Sol] = ParetoSet(2)
  pareto.Objs.foreach(pareto.nadir(_) = 5000)
  
  // Visualization
  val visu = new VisualSet(pareto.nadir(0), pareto.nadir(1))
  
  // Parsing
  val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/renA6.tsp")
  val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/renB6.tsp") 
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrices(0).size
  val Cities = 0 until nCities

  // Model
  // -----
  val cp = new CPSolver()
  cp.silent = true

  // Successors & Predecessors
  val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities))
  val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities))

  // Total distance
  val totDists = Array.tabulate(pareto.nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.minimizeMO(pareto, totDists: _*) subjectTo {
  //cp.solve() subjectTo {

    // Channeling between predecessors and successors
    cp.add(new ChannelingPredSucc(cp, pred, succ))

    // Consistency of the circuit with Strong filtering
    cp.add(circuit(succ), Strong)
    cp.add(circuit(pred), Strong)

    for (o <- pareto.Objs) {
      cp.add(sum(Cities)(i => distMatrices(o)(i)(succ(i))) == totDists(o))
      cp.add(sum(Cities)(i => distMatrices(o)(i)(pred(i))) == totDists(o))
      cp.add(new TONOTCOMMIT(cp, pred, distMatrices(o), totDists(o)))
      cp.add(new TONOTCOMMIT(cp, succ, distMatrices(o), totDists(o)))
    }
  }
  
  println("Search...")

  cp.exploration {
    
    val dist = distMatrices(cp.random.nextInt(pareto.nObjs))
    
    while (!allBounds(succ)) {
        
      for (i <- 0 to 100000000) i*0.4
      visu.line(totDists(0).min, 0)
      visu.line(totDists(1).min, 1)
      visu.line(totDists(0).max, 2)
      visu.line(totDists(1).max, 3)
      
      val i = selectMin(0 until succ.size)(!succ(_).isBound)(succ(_).size).get
      val j = selectMin(0 until succ.size)(succ(i).hasValue(_))(dist(i)(_)).get

      cp.branch(cp.post(succ(i) == j))(cp.post(succ(i) != j))
    }
    
    solFound()
  } 
  
  def solFound() {
    println("Sol found: " + totDists.map(_.value).mkString(", "))    
    val newSol = MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDists.map(_.value))
    pareto.insert(newSol)
    visu.update(pareto.map(s => (s.objs(0), s.objs(1))).toArray)
  }
  
  cp.run()
  println("Pareto Set:")
  println(pareto.sortedByObj(0).mkString("\n"))
}