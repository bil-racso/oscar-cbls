package oscar.cp.mem.tsp

import oscar.cp.mem.visu.PlotPareto
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.pareto.MOSol
import scala.collection.mutable.Queue
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.mem.InSet
import oscar.cp.modeling.CPSolver
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.measures.Hypervolume.hypervolume
import oscar.cp.mem.RoutingUtils.regretHeuristic
import oscar.cp.mem.DynDominanceConstraint
import oscar.cp.constraints.MinAssignment

object MoLnsTSP extends App {

  case class Sol(pred: Array[Int], succ: Array[Int]) { var tabu = 0 }

  val pareto: ParetoSet[Sol] = ParetoSet(2)
  val newSols: ParetoSet[Sol] = ParetoSet(pareto.nObjs)
  pareto.Objs.foreach(pareto.nadir(_) = 180000)
  
  val visu = new PlotPareto(pareto)
  
  val inst1 = "A"
  val inst2 = "B"

  val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/kro"+inst1+"100.tsp")
  val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/kro"+inst2+"100.tsp")
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities

  // First Phase
  // -----------
  
  // Read data
  val preds = TSPUtils.readSet("firstPhase"+inst1+inst2+"merged.txt")
  val succs = TSPUtils.buildSuccsFromPreds(preds)

  // Only use edges of the first phase
  val selected = Array.fill(distMatrix1.size)(Array.fill(distMatrix1.size)(false))
  for (pred <- preds; i <- 0 until pred.size) {
    selected(i)(pred(i)) = true
    selected(pred(i))(i) = true
  }

  // Insert first points
  for (i <- 0 until preds.size) {
    val dist1 = TSPUtils.computeDist(preds(i), distMatrix1)
    val dist2 = TSPUtils.computeDist(preds(i), distMatrix2)
    val x = MOSol(Sol(preds(i), succs(i)), dist1, dist2)
    pareto insert x
  }  
  
  println(pareto.sortByObj(0).mkString("\n"))
  
  // Init Model
  // ----------
  
  val cp = new CPSolver()
  cp.silent = true

  // Successors & Predecessors  
  val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))
  val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))

  // Total distance
  val totDists = Array.tabulate(pareto.nObjs)(o => CPVarInt(cp, 0 to distMatrices(o).flatten.sum))

  // Constraints
  // -----------
  cp.minimize(totDists:_*) subjectTo {

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
  var currentObjective = 0
  cp.exploration {    
    regretHeuristic(cp, succ, distMatrices(currentObjective))
    solFound()
  } 

  // MOLNS Framework
  // ---------------

  val rand = cp.random

  // Parameters
  val p = 10
  val intensFreq = 0.3
  val tabuLength = 50
 
  var currentSol: MOSol[Sol] = null
  var currentDominated = false  
  var noSol = true 
  
  // Framework 
  for (iter <- 1 to 10000) {

    if (iter % 100 == 0) println("Iter: " + iter + "\t#Set: " + pareto.size + "\tH: " + hypervolume(pareto))
    
    // Selects mode
    val intens = rand.nextFloat() < intensFreq
    
    // Selects a solution
    currentSol = selectSolution(iter, intens)
    
    // Visu
    //visu.highlight(currentSol(0), currentSol(1))
    
    // Init Search
    currentDominated = false
    noSol = true
    
    // Searches
    for (o <- pareto.Objs if !currentDominated) {
      currentObjective = o
      cp.runSubjectTo(failureLimit = 3000) {
        relaxObjectives(o, currentSol, intens)
        relaxVariables(clusterRelax(p, o), currentSol)
      }
    }
    
    // Tabu list
    if (noSol) currentSol.tabu = iter + tabuLength
  }

  def selectSolution(iter: Int, intens: Boolean): MOSol[Sol] = {  
    val notTabu = pareto.filter(_.tabu <= iter)
    if (!notTabu.isEmpty) notTabu.sortBy(surfHeuristic(_, intens)).last
    else {
      val min = pareto.min(_.tabu)
      val diff = min.tabu - iter
      pareto.foreach(p => p.tabu = p.tabu - diff)
      min
    }
  }
  
  def surfHeuristic(sol: MOSol[Sol], intens: Boolean): Int = {
    if (intens) computeLIS(sol)
    else computeLDS(sol)
  }

  def computeLIS(sol: MOSol[Sol]): Int = {
    val diff1 = sol(0) - sol.lowerBound(0)
    val diff2 = sol(1) - sol.lowerBound(1)
    diff1 * diff2
  }

  def computeLDS(sol: MOSol[Sol]): Int = {
    val diff12 = sol.upperBound(0) - sol(0)
    val diff11 = sol(0) - sol.lowerBound(0)
    val diff22 = sol.upperBound(1) - sol(1)
    val diff21 = sol(1) - sol.lowerBound(1)
    diff12 * diff21 + diff11 * diff22
  }

  def relaxObjectives(obj: Int, sol: MOSol[Sol], intensification: Boolean = false) {   
    for (o <- pareto.Objs) {
      if (o != obj) {
        cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
        if (intensification) cp.objective.objs(o).best = sol.upperBound(o)
        else cp.objective.objs(o).best = sol(o)
      } else {
        cp.objective.objs(o).tightenMode = TightenType.StrongTighten
        cp.objective.objs(o).best = sol(o)
      }
    }
  }

  def clusterRelax(p: Int, obj: Int): Array[Boolean] = {
    val c = rand.nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrices(obj)(c)(i))
    val dist = distMatrices(obj)(c)(sortedByDist(p))
    Array.tabulate(nCities)(i => distMatrices(obj)(c)(i) <= dist)
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
  }
  
  def solFound() {   
    
    // No dominated solutions !
    val newSol = MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDists.map(_.value))  
    assert(pareto.insert(newSol) != -1) 
       
    if (newSol dominates currentSol) currentDominated = true   
    noSol = false
    
    // Visu
    visu.update()
  }
}