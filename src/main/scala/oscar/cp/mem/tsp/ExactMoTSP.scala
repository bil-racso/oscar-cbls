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
import scala.collection.mutable.Queue
import oscar.cp.mem.DynDominanceConstraint

object ExactMoTSP extends App {

  case class Sol(pred: Array[Int], succ: Array[Int])

  // BiObjective Pareto Set 
  val pareto: ParetoSet[Sol] = ParetoSet(2)
  pareto.Objs.foreach(pareto.nadir(_) = 10000)
  
  // Visualization
  val visu = new VisualSet(pareto)
  
  // Parsing
  val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/renA15.tsp")
  val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/renB15.tsp") 
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrix1.size
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
  cp.solve() subjectTo {

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
    
    cp.add(new DynDominanceConstraint(cp, pareto, totDists:_*))
  }
  
  // Search
  // ------
  var objective = 0
  cp.exploration {    
    while(!allBounds(succ)) {
      objective = 1-objective
      val median = totDists(objective).median
      cp.branch(cp.post(totDists(objective) < median))(cp.post(totDists(objective) >= median))
    }
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
  }
  
  // Run
  // ---  
  println("Search...")
  cp.run()  
  
  /*for (iter <- 1 to 1000000) {

    cp.runSubjectTo(failureLimit = 1000) {
      
      val intens = cp.random.nextFloat < 0
      val sol = selectSol(intens)
      
      if (intens) {
        pareto.Objs.foreach(o => cp.post(totDists(o) < sol.objs(o)))
      }
      
      objective = cp.random.nextInt(pareto.nObjs)
      relaxVariables(clusterRelax(5, objective), sol)
    }
  }
  
  def clusterRelax(p: Int, obj: Int): Array[Boolean] = {
    val c = rand.nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrices(obj)(c)(i))
    val dist = distMatrices(obj)(c)(sortedByDist(p))
    Array.tabulate(nCities)(i => distMatrices(obj)(c)(i) <= dist)
  }
  
  def selectSol(intens: Boolean): MOSol[Sol] = {
    val points = pareto.toArray
    points(cp.random.nextInt(points.size))
  }

  def relaxVariables(selected: Array[Boolean], sol: MOSol[Sol]) {
    val constraints: Queue[Constraint] = Queue()
    for (c <- Cities; if !selected(c)) {
      val p = sol.sol.pred(c)
      val s = sol.sol.succ(c)
      if (!selected(p) && !selected(s)) {
        constraints.enqueue(new InSet(cp, pred(c), Set(p, s)))
        constraints.enqueue(new InSet(cp, succ(c), Set(p, s)))       
      }
    }
    val notSelected = Cities.filter(selected(_))
    val r = rand.nextInt(notSelected.size)
    val cc = notSelected(r)
    constraints.enqueue(pred(cc) == (if (cp.random.nextBoolean()) sol.pred(cc) else sol.succ(cc)))
    cp.post(constraints.toArray)
  }*/
 
  cp.printStats() 
  println("Pareto Set")
  println("H: " + oscar.cp.mem.measures.Hypervolume.hypervolume(pareto))
  println(pareto.sortedByObj(0).mkString("\n"))
}