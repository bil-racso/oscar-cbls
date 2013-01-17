package oscar.cp.mem.tap

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import oscar.reversible._
import oscar.visual._
import scala.collection.JavaConversions._
import oscar.cp.mem.pareto.MOSol
import oscar.cp.mem.pareto.NewPareto
import scala.collection.mutable.Queue

/**
 * Chemical Tanker Problem:
 * The Objective is to place products (called cargos) into tanks on a chemical tanker (vessel).
 * - At most one cargo per tank but several tanks can be used to all the volume of one cargo.
 * - Some cargo cannot be placed into adjacent tanks (different temperature requirement and security constraints)
 * - Some cargo cannot be placed into some tanks (all the tanks does not have the required property to accept the cargo)
 * The objective it to place  all the volumes while satisfying the security constraints and maximizing the total free space (total volume of unused space).
 * The idea of the objective function is to let more freedom for future cargos and also to decrease the cleaning costs
 *
 * @author Pierre Schaus  - pschaus@gmail.com
 * @author Renaud Hartert - ren.hartert@gmail.com
 */
object MolnsTAP extends App {

  // Data parsing
  // ------------------------------------------

  val instance = TAPUtils.parseInstance("data/chemical2.xml")
  val cargos = instance.cargos
  val tanks = instance.tanks
  val totCapa = instance.totCapa
  val incompatibles = instance.incompatibles
  val compatibles = instance.compatibles

  val nTanks = tanks.size
  val Tanks = 0 until nTanks
  val nCargos = cargos.size
  val Cargos = 0 until nCargos

  // Model
  // ------------------------------------------

  val cp = CPSolver()
  cp.silent = true

  // For each tank, the cargo type placed into it (dummy cargo if empty)
  val cargo = Array.tabulate(nTanks)(t => CPVarInt(cp, tanks(t).possibleCargos))

  // For each cargo, the total capacity allocated to it (must be at least the volume to place)
  val load = Array.tabulate(nCargos)(c => CPVarInt(cp, cargos(c).volume to totCapa))

  // For each cargo, the number of tanks allocated to it
  val card = Array.tabulate(nCargos)(c => CPVarInt(cp, 0 to tanks.size))

  // Objective = maximize the total empty space
  val freeSpace = load(0)
  val nbFreeTanks = card(0)

  // Tanks allocated to cargo c in current partial solution
  def tanksAllocated(c: Int) = (Tanks).filter(t => (cargo(t).isBound && cargo(t).getValue == c))

  // Volume allocated to cargo c in current partial solution
  def volumeAllocated(c: Int) = tanksAllocated(c).map(tanks(_).capa).sum

  // MOLNS
  // ------------------------------------------

  case class Sol(cargo: Array[Int]) { var tabu = 0 }
  val pareto = NewPareto[Sol](2)
  var newSols = NewPareto[Sol](2)
  var currentSol: MOSol[Sol] = null
  var currentObjective = 0
  var iteration = 0
  var firstLns = true

  val p = 25
  val tabuLength = 100
  val maxIter = 2000

  val t0 = System.currentTimeMillis()
  cp.lns(1000) {

    println("Iteration: " + iteration + " #Set: " + pareto.size)

    if (iteration == maxIter) {
      cp.stop
      println(System.currentTimeMillis() - t0)
    }

    // If first LNS, select a first solution
    if (firstLns) {
      if (!cp.startByLNS) insertNewSolutions()
      currentObjective = 0
      selectSolution()
      firstLns = false
    } // If the current solution is removed
    else if (insertNewSolutions()) {
      currentObjective = 0
      selectSolution()
    } // If all objectives have been considered
    else if (currentObjective == pareto.Objs.max) {
      currentObjective = 0
      currentSol.tabu = iteration + tabuLength
      selectSolution()
    } // Else, try next objective
    else {
      currentObjective += 1
    }

    relaxObjectives(currentObjective)
    relaxVariables(randomRelax(p))
  }

  def insertNewSolutions(): Boolean = {
    if (newSols.isEmpty) {
      false
    } else {
      var removed = false
      newSols.foreach(x => {
        if (x dominates currentSol) removed = true
        println("new sol removes " + pareto.insert(x) + " sols")
      })
      newSols.clear()
      removed
    }
  }

  def selectSolution() {
    var filteredSol = pareto.filter(_.tabu <= iteration)
    if (filteredSol.isEmpty) {
      val min = pareto.min(_.tabu)
      pareto.foreach(_.tabu -= (min.tabu - iteration))
      filteredSol = List(min)
    }
    val r = cp.random.nextInt(filteredSol.size)
    currentSol = filteredSol(r)
    iteration += 1
  }

  def relaxObjectives(obj: Int, intensification: Boolean = false) {
    for (o <- pareto.Objs) {
      if (intensification || o == obj) {
        cp.objective.objs(o).best = currentSol.objs(o)
        cp.objective.objs(o).tightenMode = TightenType.StrongTighten
      } else {
        cp.objective.objs(o).best = pareto.upper(o, currentSol.objs(o)) - 1
        cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
      }
    }
  }

  def randomRelax(p: Int): Array[Boolean] = {
    Array.tabulate(nCargos)(i => cp.random.nextInt(100) > p && !cp.isFailed())
  }

  def relaxVariables(selected: Array[Boolean]) {
    val constraints: Queue[Constraint] = Queue()
    for (i <- 0 until cargos.size; if selected(i)) {
      cp.post(cargo(i) == currentSol.cargo(i))
    }
    cp.post(constraints.toArray)
  }

  def solFound() {
    newSols.insert(MOSol(Sol(cargo.map(_.value)), -freeSpace.value, -nbFreeTanks.value))
  }

  // Search + Constraints
  // ------------------------------------------

  cp.minimize(-freeSpace, -nbFreeTanks) subjectTo {

    // Make the link between cargo and load variables with binpacking constraints
    cp.add(binPacking(cargo, tanks.map(_.capa), load), Strong)
    cp.add(binPackingCardinality(cargo, tanks.map(_.capa), load, card))

    // Dominance rules
    for (i <- 1 until nCargos)
      cp.add(new ChemicalConstraint(cargos(i), tanks, cargo))

    // Enforce that for any two neighbor tanks, they must contain compatible cargo types
    for (t <- tanks; t2 <- t.neighbours; if (t2 > t.id))
      cp.add(table(cargo(t.id - 1), cargo(t2 - 1), compatibles))
  }

  cp.exploration {

    while (!allBounds(cargo)) {
      val volumeLeft = Array.tabulate(cargos.size)(c => cargos(c).volume - volumeAllocated(c))
      val unboundTanks = cargo.zipWithIndex.filter { case (x, c) => !x.isBound }    
      
      val (tankVar, tank) = if (currentObjective == 0) {
        unboundTanks.maxBy { case (x, c) => (tanks(c).capa, -x.getSize) }
      } else {
        cargo.zipWithIndex.filter(c => !c._1.isBound).maxBy(c => (tanks(c._2).capa,-c._1.getSize))     
      }
      
      val cargoToPlace = (0 until cargos.size).filter(tankVar.hasValue(_)).maxBy(volumeLeft(_))
      cp.branch(cp.post(tankVar == cargoToPlace))(cp.post(tankVar != cargoToPlace))
    }
    
    solFound()
  }

  println("PARETO : ")
  pareto.sortBy(_.objs(0)).foreach(x => println(x.objs.mkString(" ")))
}