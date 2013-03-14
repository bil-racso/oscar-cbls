package oscar.cp.mem.tsp

import oscar.cp.mem.RoutingUtils._
import oscar.cp.modeling._
import oscar.cp.core._
import oscar.cp.mem.ChannelingPredSucc
import oscar.cp.mem.InSet
import oscar.cp.mem.pareto.ParetoSet
import oscar.cp.mem.pareto.MOSol
import oscar.cp.mem.visu.PlotPareto
import oscar.util._
import scala.collection.mutable.Queue
import oscar.cp.mem.DynDominanceConstraint
import oscar.cp.mem.measures.Hypervolume.hypervolume
import oscar.cp.constraints.MinAssignment
import oscar.cp.mem.Gavanelli02
import oscar.cp.mem.pareto.ListPareto
import oscar.cp.mem.pareto.Pareto

object newMoTSP extends App {

  case class Sol(pred: Array[Int], succ: Array[Int]) { var lifes = 4 }

  // BiObjective Pareto Set 
  val pareto: Pareto[Sol] = new ListPareto(3)
  pareto.Objs.foreach(pareto.nadir(_) = 180000)
  
  var allSol : List[Array[Int]] = List()
  
  // Parsing
  val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/renA50.tsp")
  val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/renB50.tsp") 
  val distMatrix3 = TSPUtils.buildDistMatrix("data/TSP/renC50.tsp") 
  val distMatrices = Array(distMatrix1, distMatrix2, distMatrix3)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities
  
  //val visu = new VisualPareto(pareto)

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
      cp.add(new MinAssignment(pred, distMatrices(o), totDists(o)))
      cp.add(new MinAssignment(succ, distMatrices(o), totDists(o)))
    }
    
    cp.add(Gavanelli02(pareto, totDists:_*))
  }
  
  // Search
  // ------
  var objective = 0
  cp.exploration {    
    regretHeuristic(cp, succ, distMatrices(objective))
    solFound()
  } 

  var noSol = true
  def solFound() {   
    // No dominated solutions !
    val newSol = MOSol(Sol(pred.map(_.value), succ.map(_.value)), totDists.map(_.value))    
    assert(pareto.insert(newSol) != -1) 
    noSol = false
    // Visu
    //visu.update()
    
    allSol = newSol.objVals :: allSol
  }
  
  // Run
  // ---  
  println("Search...")
  
  for (o <- pareto.Objs) {
    objective = o
    cp.run(nbSolMax = 1)
  }
  
  var stopCriterion = false
  var iter = 0
  var p = 5
  
  while(!stopCriterion) {

    iter += 1
    if (iter % 10 == 0) println("p " + p + " Iter: " + iter + "\t#Set: " + pareto.size)   

    noSol = true
    val alive = pareto.filter(_.lifes > 0)
    
    if (alive.isEmpty) stopCriterion = true
    else {
      val sol = alive(cp.random.nextInt(alive.size))
      objective = cp.random.nextInt(pareto.nObjs)

      cp.runSubjectTo(failureLimit = 2000) {
        
        if (rand.nextBoolean) {
          for (o <- pareto.Objs) {
            if (o == objective) cp.post(totDists(o) < sol(o))
            else cp.post(totDists(o) <= sol(o))
          }
        }
        
        relaxVariables(clusterRelax(p, objective), sol)
      }

      if (noSol) sol.lifes -= 1
    }
    if (iter > 1000) stopCriterion = true
    
    if (allSol.size >= 10000) {
      stopCriterion = true
      println(allSol.map("Array("+_.mkString(", ")+")"))
    }
  }
  
  def clusterRelax(p: Int, obj: Int): Array[Boolean] = {
    val c = rand.nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrices(obj)(c)(i))
    val dist = distMatrices(obj)(c)(sortedByDist(p))
    Array.tabulate(nCities)(i => distMatrices(obj)(c)(i) <= dist)
  }
  
  def selectSol: MOSol[Sol] = {
    val points = pareto.toList
    points(rand.nextInt(points.size))
    /*val value = for (p <- points) yield {
      var dist = 0
      for (p2 <- points; o <- pareto.Objs; if p2 != p) {        
        dist += math.abs(p(o) - p2(o))
      }
      (p, dist)
    }
    
    val sol = value.sortBy(-_._2)
    sol.head._1*/
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
 
  cp.printStats() 
  println("Pareto Set")
  //println("H: " + hypervolume(pareto))
  
  val points = pareto.sortByObj(0)
  //println(points.mkString("\n"))
  
  val out = OutFile("fullSetRen50")
  points.foreach(p => out.writeln(p.sol.pred.mkString(" ")))
  out.close()
}