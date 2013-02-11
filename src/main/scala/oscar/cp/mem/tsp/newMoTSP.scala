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
import oscar.cp.mem.measures.Hypervolume.hypervolume
import oscar.cp.constraints.MinAssignment

object newMoTSP extends App {

  case class Sol(pred: Array[Int], succ: Array[Int]) { var lifes = 10 }

  // BiObjective Pareto Set 
  val pareto: ParetoSet[Sol] = ParetoSet(2)
  pareto.Objs.foreach(pareto.nadir(_) = 300000)
  
  // Visualization
  val visu = new VisualSet(pareto)
  
  // Parsing
  val distMatrix1 = TSPUtils.buildDistMatrix("data/TSP/renA50.tsp")
  val distMatrix2 = TSPUtils.buildDistMatrix("data/TSP/renB50.tsp") 
  val distMatrices = Array(distMatrix1, distMatrix2)
  val nCities = distMatrix1.size
  val Cities = 0 until nCities
  
  // First Phase
  // -----------
  /*
  // Read data
  val preds = TSPUtils.readSet("firstPhaseABmerged.txt")
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
  
  val sol1 = pareto.min(_.objs(0))
  val sol2 = pareto.min(_.objs(1))
  pareto.clear()
  pareto insert sol1
  pareto insert sol2*/

  // Model
  // -----
  val cp = new CPSolver()
  cp.silent = true

  // Successors & Predecessors  
  val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities))
  val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities))
  //val succ = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))
  //val pred = Array.tabulate(nCities)(i => CPVarInt(cp, Cities.filter(j => selected(i)(j))))

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
    
    cp.add(new DynDominanceConstraint(cp, pareto, totDists:_*))
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
    visu.selected(totDists(0).value, totDists(1).value)
    visu.update()
    visu.paint
  }
  
  // Run
  // ---  
  println("Search...")
  cp.run(failureLimit = 5000)
  objective = 1
  cp.run(failureLimit = 5000)
  var stopCriterion = false
  var iter = 0
  var p = 5
  
  while(!stopCriterion) {

    iter += 1
    if (iter % 1000 == 0 && p < 10) p += 1
    if (iter % 100 == 0) println("p " + p + "Iter: " + iter + "\t#Set: " + pareto.size + "\tH: " + hypervolume(pareto))   

    noSol = true
    val alive = pareto.filter(_.lifes > 0)
    
    if (alive.isEmpty) stopCriterion = true
    else {
      val sol = alive(cp.random.nextInt(alive.size))
      objective = cp.random.nextInt(pareto.nObjs)

      cp.runSubjectTo(failureLimit = 2000) {
        relaxVariables(clusterRelax(p, objective), sol)
      }

      if (noSol) sol.lifes -= 1
    }
    if (iter > 15000) stopCriterion = true
  }
  
  def clusterRelax(p: Int, obj: Int): Array[Boolean] = {
    val c = rand.nextInt(nCities)
    val sortedByDist = Cities.sortBy(i => distMatrices(obj)(c)(i))
    val dist = distMatrices(obj)(c)(sortedByDist(p))
    Array.tabulate(nCities)(i => distMatrices(obj)(c)(i) <= dist)
  }
  
  def selectSol: MOSol[Sol] = {
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
  }
 
  cp.printStats() 
  println("Pareto Set")
  println("H: " + hypervolume(pareto))
  
  val points = pareto.sortedByObj(0)
  //println(points.mkString("\n"))
  
  val out = OutFile("fullSetRen50")
  points.foreach(p => out.writeln(p.sol.pred.mkString(" ")))
  out.close()
}