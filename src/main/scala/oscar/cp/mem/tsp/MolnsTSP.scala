package oscar.cp.mem.tsp

import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.util._
import oscar.cp.constraints._
import oscar.cp.mem.visu.VisualRelax
import scala.collection.mutable.Queue
import scala.util.Random.nextInt
import scala.util.Random.nextFloat
import scala.math.round
import oscar.search.IDSSearchController
import java.io._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.NewPareto
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.pareto.MOSol
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach
import oscar.cp.mem.visu.VisualSet
import oscar.visual.Plot2D
import oscar.visual.VisualFrame
import oscar.cp.mem.DominanceConstraint

object MolnsTSP {

  val visu = new VisualSet(180000, 180000)

  val verbous = false

  case class Sol(pred: Array[Int], succ: Array[Int]) { var tabu = 0 }
  var selected: Array[Array[Boolean]] = null
  var plot: List[(Int, Int)] = List()

  def main(args: Array[String]) {

    val inst1 = 'A'
    val inst2 = 'B'
    val in = "10c_merged"
    val out = "newTabu150"

    val pareto: ParetoSet[Sol] = ParetoSet(2)

    pareto.nadir(0) = 180000
    pareto.nadir(1) = 180000

    val preds = TSPUtils.readSet("firstPhase" + inst1 + inst2 + in + ".txt")
    val succs = TSPUtils.buildSuccsFromPreds(preds)

    val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/kro" + inst1 + "100.tsp")
    val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/kro" + inst2 + "100.tsp")

    selected = Array.fill(distMatrix1.size)(Array.fill(distMatrix1.size)(false))
    for (pred <- preds; i <- 0 until pred.size) {
      selected(i)(pred(i)) = true
      selected(pred(i))(i) = true
    }

    for (i <- 0 until preds.size) {
      val i = 1
      val dist1 = TSPUtils.computeDist(preds(i), distMatrix1)
      val dist2 = TSPUtils.computeDist(preds(i), distMatrix2)
      val x = MOSol(Sol(preds(i), succs(i)), dist1, dist2)
      pareto insert x
    }

    solveMO(pareto, distMatrix1, distMatrix2)

    println("" + inst1 + inst2 + out)

    TSPUtils.writeSet("setSol" + inst1 + inst2 + out + ".txt", pareto.toArray.map(_.sol.pred))
    val outFile = OutFile("Molns" + inst1 + inst2 + out + ".txt")
    pareto.foreach(x => outFile.writeln(x.objs.mkString(" ")))
    outFile.close()

    val outFile2 = OutFile("Size" + inst1 + inst2 + out + ".txt")
    plot.foreach(x => outFile2.writeln(x._1 + " " + x._2))
    outFile2.close()
  }

  def hypervolume(set: ParetoSet[Sol]): Double = {
    var prevObj2 = set.nadir(1)
    var volume = 0.0
    val sortedSet = set.sortedByObj(0)
    for (s <- sortedSet) {
      val obj1 = s.objs(0)
      val obj2 = s.objs(1)
      val dObj2 = prevObj2 - obj2
      val dObj1 = set.nadir(0) - obj1
      prevObj2 = obj2
      val v = ((dObj2.toDouble / 10000) * (dObj1.toDouble / 10000))
      if (v > 0) volume += v
    }
    volume
  }

  def solveMO(pareto: ParetoSet[Sol], distMatrix1: Array[Array[Int]], distMatrix2: Array[Array[Int]]) = {

    val nCities = distMatrix1.size
    val Cities = 0 until nCities

    // Model
    // -----
    val cp = new CPSolver()
    cp.silent = true

    // Successors & Predecessors
    val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))
    val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))

    // Total distance
    val totDist1 = CPVarInt(cp, 0 to distMatrix1.flatten.sum)
    val totDist2 = CPVarInt(cp, 0 to distMatrix2.flatten.sum)

    // Constraints
    // -----------
    cp.minimize(totDist1, totDist2) subjectTo {

      // Channeling between predecessors and successors
      cp.add(new ChannelingPredSucc(cp, pred, succ))

      // Consistency of the circuit with Strong filtering
      cp.add(circuit(succ))//, Strong)
      cp.add(circuit(pred))//, Strong)

      // Total distance 1
      cp.add(sum(Cities)(i => distMatrix1(i)(succ(i))) == totDist1)
      cp.add(sum(Cities)(i => distMatrix1(i)(pred(i))) == totDist1)

      cp.add(new TONOTCOMMIT(cp, pred, distMatrix1, totDist1))
      cp.add(new TONOTCOMMIT(cp, succ, distMatrix1, totDist1))

      // Total distance 2
      cp.add(sum(Cities)(i => distMatrix2(i)(succ(i))) == totDist2)
      cp.add(sum(Cities)(i => distMatrix2(i)(pred(i))) == totDist2)

      cp.add(new TONOTCOMMIT(cp, pred, distMatrix2, totDist2))
      cp.add(new TONOTCOMMIT(cp, succ, distMatrix2, totDist2))
    }
    
    // -----
    var newSols = NewPareto[Sol](2)
    var currentSol: MOSol[Sol] = null
    
        
    def solFound() {
      newSols.insert(MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDist1.value, totDist2.value))
    }
    var currentObj = 0

    // Search
    // ------
    println("Searching...")
    cp.exploration {
      val dist = if (currentObj == 0) distMatrix1 else distMatrix2

      visu.line(totDist1.min, 0)
      visu.line(totDist2.min, 1)
      visu.line(totDist1.max, 2)
      visu.line(totDist2.max, 3)

      regretHeuristic(cp, succ, dist)
      solFound()
    }
    
    // MOLNS
    var newPoint = false

    val p = 10
    val tabuLength = 150
    var cycleBreaker = false
    val maxIter = 100000
    val intensFreq = 0.3

    val t0 = System.currentTimeMillis()
    
    for (iter <- 1 to maxIter) {
            
      if (iter % 100 == 0) printStats(iter)
      if (verbous) println("Iteration: " + iter + " #Set: " + pareto.size)
           
      val intens = cp.random.nextFloat() < intensFreq
      selectSolution(iter, intens)
      
      newPoint = false
      for (obj <- pareto.Objs if !newPoint) {
        
        currentObj = obj
        newSols.clear
        
        cp.runSubjectTo(Int.MaxValue, 500) {
          
          visu.selected((currentSol.objs(0), currentSol.objs(1)))
          relaxObjectives(currentObj, intens)
          relaxVariables(clusterRelax(p, currentObj))
        }
        
        newPoint = insertNewSolutions()       
      }
    }
    
    def printStats(iter: Int) {
      println("Iteration: " + iter + " #Set: " + pareto.size)
      println("H: " + hypervolume(pareto))
    }

    def insertNewSolutions(): Boolean = {
      if (newSols.isEmpty) {
        false
      } else {
        var removed = false
        newSols.foreach(x => {
          if (x dominates currentSol) removed = true
          val n = pareto.insert(x)
          if (verbous) println("new sol removes " + n + " sols")

          visu.update(pareto.map(p => (p.objs(0), p.objs(1))).toArray)
        })
        newSols.clear()

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
      val rand = math.floor(math.pow(cp.random.nextFloat(), 10) * sorted.size).toInt
      currentSol = sorted(rand)
    }

    def computeIntenSurf(sol: MOSol[Sol]): Int = {
      val diff1 = sol.objs(0) - sol.lowerBound(0)
      val diff2 = sol.objs(1) - sol.lowerBound(1)
      diff1 * diff2
    }

    def computeDiffSurf(sol: MOSol[Sol]): Int = {

      val obj12 = sol.upperBound(0) - sol.objs(0)
      val obj11 = sol.objs(0) - sol.lowerBound(0)
      val obj22 = sol.upperBound(1) - sol.objs(1)
      val obj21 = sol.objs(1) - sol.lowerBound(1)

      obj12 * obj21 + obj11 * obj22
    }

    def relaxObjectives(obj: Int, intensification: Boolean = false) {

      if (!intensification) {

        for (o <- pareto.Objs) {
          if (o != obj) {
            cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
            cp.objective.objs(o).best = pareto.nadir(o)
          } else {
            cp.objective.objs(o).tightenMode = TightenType.StrongTighten
            cp.objective.objs(o).best = currentSol.objs(o)
          }
        }

        cp.post(new DominanceConstraint(cp, pareto, totDist1, totDist2))
        
      } else {
        
        for (o <- pareto.Objs) {
          cp.objective.objs(o).best = currentSol.objs(o)
          if (o == obj) {
            cp.objective.objs(o).tightenMode = TightenType.StrongTighten
          } else {
            cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
          }
        }

      }
    }

    def clusterRelax(p: Int, obj: Int): Array[Boolean] = {

      val distMatrix = if (obj == 0) distMatrix1 else distMatrix2

      val c = nextInt(nCities)
      val sortedByDist = Cities.sortBy(i => distMatrix(c)(i))
      val dist = distMatrix(c)(sortedByDist(p))

      Array.tabulate(nCities)(i => distMatrix(c)(i) <= dist)
    }

    def relaxVariables(selected: Array[Boolean]) {

      val constraints: Queue[Constraint] = Queue()

      for (c <- Cities; if !selected(c)) {
        val p = currentSol.sol.pred(c)
        val s = currentSol.sol.succ(c)
        if (!selected(p) && !selected(s)) {
          if (cycleBreaker) {
            constraints.enqueue(new InSet(cp, pred(c), Set(p, s)))
            constraints.enqueue(new InSet(cp, succ(c), Set(p, s)))
          } else {
            constraints.enqueue(pred(c) == p)
            constraints.enqueue(succ(c) == s)
          }
        }
      }

      if (cycleBreaker) {
        val notSelected = Cities.filter(selected(_))
        val rand = cp.random.nextInt(notSelected.size)
        val cc = notSelected(rand)
        constraints.enqueue(pred(cc) == (if (cp.random.nextBoolean()) currentSol.pred(cc) else currentSol.succ(cc)))
      }

      cp.post(constraints.toArray)
    }
  }
}