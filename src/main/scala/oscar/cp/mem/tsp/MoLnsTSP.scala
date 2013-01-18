package oscar.cp.mem.tsp

import oscar.cp.mem.visu.VisualSet
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.pareto.MOSol
import scala.collection.mutable.Queue
import oscar.cp.core.Constraint
import oscar.cp.modeling.TightenType
import oscar.cp.mem.DominanceConstraint
import oscar.cp.mem.InSet

object MoLnsTSP extends App {

  val verbous = false

  case class Sol(pred: Array[Int], succ: Array[Int]) { var tabu = 0 }

  val pareto: ParetoSet[Sol] = ParetoSet(2)
  val newSols: ParetoSet[Sol] = ParetoSet(pareto.nObjs)
  pareto.Objs.foreach(pareto.nadir(_) = 180000)
  
  val visu = new VisualSet(pareto.nadir(0), pareto.nadir(1))
  
  val inst1 = "A"
  val inst2 = "B"

  val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/kro"+inst1+"100.tsp")
  val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/kro"+inst2+"100.tsp")

  // First Phase
  // -----------
  
  // Read data
  val preds = TSPUtils.readSet("firstSetABfloor.txt")
  val succs = TSPUtils.buildSuccsFromPreds(preds)

  // Only use edges of the first phase
  val selected = Array.fill(distMatrix1.size)(Array.fill(distMatrix1.size)(false))
  for (pred <- preds; i <- 0 until pred.size) {
    selected(i)(pred(i)) = true
    selected(pred(i))(i) = true
  }

  // Insert first points
  for (i <- 0 until preds.size) {
    //val i = 1
    val dist1 = TSPUtils.computeDist(preds(i), distMatrix1)
    val dist2 = TSPUtils.computeDist(preds(i), distMatrix2)
    val x = MOSol(Sol(preds(i), succs(i)), dist1, dist2)
    pareto insert x
  }  
  
  // Init Model
  // ----------
  
  val bTSP = new MOTSP(selected, distMatrix1, distMatrix2) {
    def solFound() {
      newSols.insert(MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDists.map(_.value)))
    }
  }

  // MOLNS Framework
  // ---------------

  val rand = bTSP.cp.random
  var currentSol: MOSol[Sol] = null
  var currentDominated = false

  // Parameters
  val p = 10
  val tabuLength = 20
  val maxIter = 10000
  val intensFreq = 0.1
  val cycleBreaker = false

  // Framework 
  for (iter <- 1 to maxIter) {
    
    if (iter % 100 == 0) println("Iter: " + iter + "\t#Set: " + pareto.size + "\tH: " + oscar.cp.mem.measures.Hypervolume.hypervolume(pareto))
    
    // Prints some statistics
    if (verbous) println("Iteration: " + iter + " #Set: " + pareto.size)
    // Selects mode
    val intens = rand.nextFloat() < intensFreq
    // Selects a solution
    selectSolution(iter, intens)
    // Iteration
    currentDominated = false
    newSols.clear()
    for (o <- pareto.Objs if !currentDominated) {
      // Sets selected objective
      bTSP.currentObjective = o
      // New run
      bTSP.cp.runSubjectTo(Int.MaxValue, 500) {
        visu.selected((currentSol.objs(0), currentSol.objs(1)))
        relaxObjectives(o, intens)
        relaxVariables(clusterRelax(p, o), intens)
      }
      // Insert solutions
      if (newSols.isEmpty) currentSol.tabu += tabuLength
      else currentDominated = insertNewSolutions()
    }
  }

  def insertNewSolutions(): Boolean = {
    if (newSols.isEmpty) {
      false
    } else {
      var removed = false
      newSols.foreach(x => {
        if (x dominates currentSol) removed = true
        val n = pareto.insert(x)
        assert(n >= 0)
        if (verbous) println("new sol removes " + n + " sols")
        visu.update(pareto.map(p => (p.objs(0), p.objs(1))).toArray)
      })
      removed
    }
  }

  def selectSolution(iter: Int, intens: Boolean) {
    // Filter Tabu 
    var filteredSol = pareto.filter(_.tabu <= iter)
    if (filteredSol.isEmpty) {
      val min = pareto.min(_.tabu)
      pareto.foreach(_.tabu -= (min.tabu - iter))
      filteredSol = List(min)
    }
    // Diff / Intens Search
    val sorted = filteredSol.sortBy(s => if (intens) -computeIntenSurf(s) else -computeDiffSurf(s))
    val r = math.floor(math.pow(rand.nextFloat(), 5) * sorted.size).toInt
    currentSol = sorted(r)
  }

  def computeIntenSurf(sol: MOSol[Sol]): Int = {
    val diff1 = sol.objs(0) - sol.lowerBound(0)
    val diff2 = sol.objs(1) - sol.lowerBound(1)
    diff1 * diff2
  }

  def computeDiffSurf(sol: MOSol[Sol]): Int = {
    val diff12 = sol.upperBound(0) - sol.objs(0)
    val diff11 = sol.objs(0) - sol.lowerBound(0)
    val diff22 = sol.upperBound(1) - sol.objs(1)
    val diff21 = sol.objs(1) - sol.lowerBound(1)
    diff12 * diff21 + diff11 * diff22
  }

  def relaxObjectives(obj: Int, intensification: Boolean = false) {
    if (!intensification) {
      for (o <- pareto.Objs) {
        if (o != obj) {
          bTSP.cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
          bTSP.cp.objective.objs(o).best = pareto.nadir(o)
        } else {
          bTSP.cp.objective.objs(o).tightenMode = TightenType.StrongTighten
          bTSP.cp.objective.objs(o).best = currentSol.objs(o)
        }
      }
      bTSP.cp.post(new DominanceConstraint(bTSP.cp, pareto, bTSP.totDists:_*))
    } 
    else {
      for (o <- pareto.Objs) {
        bTSP.cp.objective.objs(o).best = currentSol.objs(o)
        if (o == obj) {
          bTSP.cp.objective.objs(o).tightenMode = TightenType.StrongTighten
        } else {
          bTSP.cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
        }
      }
    }
  }

  def clusterRelax(p: Int, obj: Int): Array[Boolean] = {
    val distMatrix = if (obj == 0) distMatrix1 else distMatrix2
    val c = rand.nextInt(bTSP.nCities)
    val sortedByDist = bTSP.Cities.sortBy(i => distMatrix(c)(i))
    val dist = distMatrix(c)(sortedByDist(p))
    Array.tabulate(bTSP.nCities)(i => distMatrix(c)(i) <= dist)
  }

  def relaxVariables(selected: Array[Boolean], intens: Boolean) {
    val constraints: Queue[Constraint] = Queue()
    for (c <- bTSP.Cities; if !selected(c)) {
      val p = currentSol.sol.pred(c)
      val s = currentSol.sol.succ(c)
      if (!selected(p) && !selected(s)) {
        if (intens) {
          constraints.enqueue(new InSet(bTSP.cp, bTSP.pred(c), Set(p, s)))
          constraints.enqueue(new InSet(bTSP.cp, bTSP.succ(c), Set(p, s)))
        } else {
          constraints.enqueue(bTSP.pred(c) == p)
          constraints.enqueue(bTSP.succ(c) == s)
        }
      }
    }

    if (intens) {
      val notSelected = bTSP.Cities.filter(selected(_))
      val r = rand.nextInt(notSelected.size)
      val cc = notSelected(r)
      constraints.enqueue(bTSP.pred(cc) == (if (rand.nextBoolean()) currentSol.pred(cc) else currentSol.succ(cc)))
    }

    bTSP.cp.post(constraints.toArray)
  }
}