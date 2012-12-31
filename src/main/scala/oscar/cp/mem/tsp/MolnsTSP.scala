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
import oscar.cp.mem.pareto.ParetoMinSet
import java.io._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.NewPareto
import oscar.cp.mem.pareto.MOSol
import com.sun.org.apache.xalan.internal.xsltc.compiler.ForEach

object MolnsTSP {

  case class Sol(pred: Array[Int], succ: Array[Int]) { var tabu = 0 }
  var selected : Array[Array[Boolean]] = null
  
  def main(args: Array[String]) {
    
    val inst1 = 'A'
    val inst2 = 'B'
    val in =  "Good" 
    val out = "10Bunch"
    
    val pareto: NewPareto[Sol] = NewPareto(2)
    
    val preds = TSPUtils.readSet("firstPhase"+inst1+inst2+in+".txt")
    val succs = TSPUtils.buildSuccsFromPreds(preds)
    
    val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/kro"+inst1+"100.tsp")
    val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/kro"+inst2+"100.tsp")
    
    selected = Array.fill(distMatrix1.size)(Array.fill(distMatrix1.size)(false))
    for (pred <- preds; i <- 0 until pred.size) {
      selected(i)(pred(i)) = true
      selected(pred(i))(i) = true
    }
    
    for (i <- 0 until preds.size) {
      val dist1 = TSPUtils.computeDist(preds(i), distMatrix1)
      val dist2 = TSPUtils.computeDist(preds(i), distMatrix2)
      val x = MOSol(Sol(preds(i), succs(i)), dist1, dist2)
      pareto insert x
    }

    solveMO(pareto, distMatrix1, distMatrix2)

    TSPUtils.writeSet("setSol"+inst1+inst2+out+".txt", pareto.toArray.map(_.sol.pred))
    
    val outFile = OutFile("setPoint"+inst1+inst2+out+".txt")
    pareto.foreach(x => outFile.writeln(x.objs.mkString(" ")))
    outFile.close()
  }

  def solveMO(pareto: NewPareto[Sol], distMatrix1: Array[Array[Int]], distMatrix2: Array[Array[Int]]) = {

    val nCities = distMatrix1.size
    val Cities = 0 until nCities

    // Model
    // -----
    val cp = new CPSolver()
    cp.silent = true
    cp.startByLNS = true

    // Successors & Predecessors
    //val succ = Array.fill(nCities)(CPVarInt(cp, Cities))
    //val pred = Array.fill(nCities)(CPVarInt(cp, Cities))
    val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))
    val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))

    // Total distance
    val totDist1 = CPVarInt(cp, 0 to distMatrix1.flatten.sum)
    val totDist2 = CPVarInt(cp, 0 to distMatrix2.flatten.sum)

    // MOLNS
    // -----
    var newSols = NewPareto[Sol](2)
    var currentSol: MOSol[Sol] = null
    var currentObjective = 0
    var iteration = 0
    var firstLns = true
    
    var p = 10
    val tabuLength = 200
    var cycleBreaker = true
    val maxIter = 10000

    val t0 = System.currentTimeMillis()
    cp.lns(500) {  
      
      println("Iteration: " + iteration + " #Set: " + pareto.size)
      
      if (iteration == maxIter) {
        cp.stop
        println(System.currentTimeMillis() - t0)
      }
      else if (iteration > 7500) {
        cycleBreaker = true
        if (nextInt(5) == 0) p = 30
        else p = 10
      }
      
      // If first LNS, select a first solution
      if (firstLns) {
        currentObjective = 0
        selectSolution()
        firstLns = false
      }
      // If the current solution is removed
      else if (insertNewSolutions()) {
        currentObjective = 0
        selectSolution()
      }
      // If all objectives have been considered
      else if (currentObjective == pareto.Objs.max) {
        currentObjective = 0
        selectSolution()
      }
      // Else, try next objective
      else {       
        currentObjective += 1      
      }
      relaxObjectives(currentObjective)
      relaxVariables(clusterRelax(p))
    }
    
    def insertNewSolutions(): Boolean = {
      if (newSols.isEmpty) {
        false
      }
      else {
        var removed = false
        newSols.foreach(x => {
          if (x dominates currentSol) removed = true
          println("new sol removed : " + pareto.insert(x))
        })
        newSols.clear()
        removed
      }
    }

    def selectSolution() { 
      var filteredSol = pareto.filter(_.tabu <= iteration)  
      if (filteredSol.isEmpty) {
        val min = pareto.min(_.tabu).tabu
        pareto.foreach(_.tabu -= min)
        filteredSol = pareto.filter(_.tabu <= iteration)  
      }
      val r = nextInt(filteredSol.size)
      currentSol = filteredSol(r)
      //val r = nextInt(pareto.size)
      //currentSol = pareto(r)
      iteration += 1       
    }
    
    def relaxObjectives(obj: Int, intensification: Boolean = false) {
      for (o <- pareto.Objs) {
        if (intensification || o == obj) {
          cp.objective.objs(o).best = currentSol.objs(o)
          cp.objective.objs(o).tightenMode = TightenType.StrongTighten
        }
        else {
          cp.objective.objs(o).best = pareto.upper(o, currentSol.objs(o)) - 1
          cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
        }
      }
    }

    def clusterRelax(p: Int): Array[Boolean] = {
      
      val distMatrix = if (currentObjective == 0) distMatrix1 else distMatrix2
      
      val c = nextInt(nCities)
      val sortedByDist = Cities.sortBy(i => distMatrix(c)(i))
      val dist = distMatrix(c)(sortedByDist(p))

      Array.tabulate(nCities)(i => distMatrix(c)(i) <= dist)
    }
    
    def proximityRelax(p: Int): Array[Boolean] = {
      
      val dist = if (currentObjective == 0) distMatrix1 else distMatrix2
      val selected = Array.fill(nCities)(false)
      selected(nextInt(nCities)) = true

      for (i <- 1 until p) {
        val sel = Cities.filter(i => selected(i))
        val rem = Cities.filter(i => !selected(i))     
        val c = sel(nextInt(sel.size))
        val cNew = selectMin(rem)()(dist(c)(_)).get
        selected(cNew) = true
      }
      selected
    }

    def solFound() {
      newSols.insert(MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDist1.value, totDist2.value))
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
      cp.post(constraints.toArray)
    }

    // Constraints
    // -----------
    cp.minimize(totDist1, totDist2) subjectTo {

      // Channeling between predecessors and successors
      cp.add(new ChannelingPredSucc(cp, pred, succ))

      // Consistency of the circuit with Strong filtering
      cp.add(circuit(succ), Strong)
      cp.add(circuit(pred), Strong)

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

    // Search
    // ------
    println("Searching...")
    cp.exploration {
      val distMatrix = if (currentObjective == 0) distMatrix1 else distMatrix2
      regretHeuristic(cp, succ, distMatrix)
      solFound()
    }
  }
}