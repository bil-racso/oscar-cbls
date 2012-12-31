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
import scala.util.Random.nextInt

/**
 * Chemical Tanker Problem:
 * The Objective is to place products (called cargos) into tanks on a chemical tanker (vessel).
 * - At most one cargo per tank but several tanks can be used to all the volume of one cargo.
 * - Some cargo cannot be placed into adjacent tanks (different temperature requirement and security constraints)
 * - Some cargo cannot be placed into some tanks (all the tanks does not have the required property to accept the cargo)
 * The objective it to place  all the volumes while satisfying the security constraints and maximizing the total free space (total volume of unused space).
 * The idea of the objective function is to let more freedom for future cargos and also to decrease the cleaning costs
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object MolnsTAP extends App {

  /**
   * Class representing a cargo object and its related data.
   * The constructor parses the xml cargo node
   */
  class Cargo(node: scala.xml.Node, val color: java.awt.Color = VisualUtil.getRandomColor()) {
    val id = (node \ "@id").text.toInt
    val name = (node \ "@name").text
    val volume = (node \ "@volume").text.toInt
    override def toString = id + ""
  }

  /**
   * Class representing a tank object and its related data.
   * The constructor parses the xml tank node
   */
  class Tank(node: scala.xml.Node, cargos: Array[Cargo]) {
    val id = (node \ "@id").text.toInt
    val capa = (node \ "@capa").text.toInt
    val x = (node \ "@x").text.toInt
    val y = (node \ "@y").text.toInt
    val w = (node \ "@w").text.toInt
    val h = (node \ "@h").text.toInt
    val impossibleCargos =
      for (n <- (node \ "impossiblecargos" \ "cargo").toArray)
        yield (n \ "@id").text.toInt
    val neighbours =
      for (n <- (node \ "neighbours" \ "tank").toArray)
        yield (n \ "@id").text.toInt
    val possibleCargos = (0 until cargos.size).filter(!impossibleCargos.contains(_)).toSet
  }

  /**
   * Constraint Enforcing dominance rules of the Chemical Tanker Problem:
   * Since we try to maximize the total free space, as soon as the total capacity
   * allocated to cargo exceed the volume of this cargo to place we immediately
   * forbid this cargo in other tanks.
   */
  class ChemicalConstraint(val cargo: Cargo, val tanks: Array[Tank], val cargos: Array[CPVarInt]) extends Constraint(cargos(0).s) {

    val curCapa = new ReversibleInt(s, 0)

    override def setup(l: CPPropagStrength) = {
      cargos.zipWithIndex.foreach(e => e._1.callValBindIdxWhenBind(this, e._2))
      CPOutcome.Suspend
    }

    override def valBindIdx(x: CPVarInt, tank: Int) = {
      if (x.getValue == cargo.id) {
        curCapa.setValue(curCapa.getValue + tanks(tank).capa)
        if (curCapa.getValue >= cargo.volume) {
          // the volume is reached for the cargo so we prevent any other tank to take this cargo
          for (c <- cargos; if (!c.isBound)) {
            c.removeValue(cargo.id) // should never fail here
          }
          CPOutcome.Success
        } else {
          CPOutcome.Suspend
        }
      } else {
        CPOutcome.Suspend
      }
    }
  }

  // ------------- parses the data of the problem  ---------------

  val problemNode = xml.XML.loadFile("data/chemical.xml")
  val dummyCargo = new Cargo(<cargo id="0" name="empty" volume="0"/>, java.awt.Color.WHITE)
  val cargos = Array(dummyCargo) ++ // dummy cargo
    (for (node <- (problemNode \ "cargos" \ "cargo").toArray)
      yield new Cargo(node))
  val tanks =
    for (node <- (problemNode \ "tanks" \ "tank").toArray)
      yield new Tank(node, cargos)

  val totCapa = (0 /: tanks)((s, t) => s + t.capa) // fold left to compute tot capa

  // extract cargo that cannot be be adjacent to each others
  val incompatibles: Set[(Int, Int)] =
    (for (n <- (problemNode \ "incompatibles" \ "incompatible"))
      yield ((n \ "@cargo1").text.toInt, (n \ "@cargo2").text.toInt)).toSet
  // transform this information to get the possible adjacent pairs
  val compatibles =
    (for (
      i <- 0 until cargos.size;
      j <- 0 until cargos.size;
      if (!incompatibles.contains((i, j)) &&
        !incompatibles.contains((j, i)))
    ) yield (i, j)).toSet

  // ------------- declare the variables of the problem ---------------

  // create the bar chart with the volume to load and volume slacks		            
  val barChart = new BarChart("", "Cargos", "Volume", Array("Volume", "Slack"), (0 until cargos.size).map("" + _).toArray, true)
  cargos.zipWithIndex.foreach { case (c, i) => barChart.setValue("Volume", i.toString, c.volume) }

  val cp = CPSolver()
  // for each tank, the cargo type placed into it (dummy cargo if emmty)
  val cargo = Array.tabulate(tanks.size)(t => CPVarInt(cp, tanks(t).possibleCargos))
  // for each cargo, the total cacity allocated to it (must be at least the volume to place)
  val load = Array.tabulate(cargos.size)(c => CPVarInt(cp, cargos(c).volume to totCapa))
  // for each cargo, the number of tanks allocated to it
  val card = Array.tabulate(cargos.size)(c => CPVarInt(cp, 0 to tanks.size))

  // objective = maximize the total empty space
  val freeSpace = load(0)
  val nbFreeTanks = card(0)

  // tanks allocated to cargo c in current partial solution
  def tanksAllocated(c: Int) = (0 until tanks.size).filter(t => (cargo(t).isBound && cargo(t).getValue == c))
  // volume allocated to cargo c in current partial solution
  def volumeAllocated(c: Int) = tanksAllocated(c).map(tanks(_).capa).sum

  val cargosol = Array.tabulate(cargo.size)(i => 0)

  val rnd = new scala.util.Random(0)
  // ask to have a 100 LNS restarts every 50 backtracks

  var nbSol = 0
  val slack = Array.tabulate(cargos.size)(c => load(0) - cargos(c).volume)

  // MOLNS
  // -----
  case class Sol(cargos: Array[Int]) { var tabu = 0 }
  val pareto = NewPareto[Sol](2)
  var newSols = NewPareto[Sol](2)
  var currentSol: MOSol[Sol] = null
  var currentObjective = 0
  var iteration = 0
  var firstLns = true

  val p = 50
  val tabuLength = 200
  val maxIter = 10000

  cp.lns(300) {
    println("Iteration: " + iteration + " #Set: " + pareto.size)
    if (iteration == maxIter) cp.stop

    // If first LNS, select a first solution
    if (firstLns) {
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
      } else {
        cp.objective.objs(o).best = pareto.upper(o, currentSol.objs(o)) - 1
        cp.objective.objs(o).tightenMode = TightenType.MaintainTighten
      }
    }
  }

  def solFound() {
    newSols.insert(MOSol(Sol(cargo.map(_.value))))
  }
  
  def randomRelax(p: Int): Array[Boolean] = {
    Array.tabulate(cargos.size)(i => rnd.nextInt(100) > 10 && !cp.isFailed())
  }

  def relaxVariables(selected: Array[Boolean]) {
    val constraints: Queue[Constraint] = Queue()
    for (i <- 0 until cargos.size; if selected(i)) {
      cp.post(cargo(i) == cargosol(i))
    }    
    cp.post(constraints.toArray)
  }

  // --------------- state the objective, the constraints and the search -------------

  cp.maximize(freeSpace, nbFreeTanks) subjectTo {
    // make the link between cargo and load vars with binpacking constraint
    cp.add(binpacking(cargo, tanks.map(_.capa), load), Strong)
    cp.add(binpackingCardinality(cargo, tanks.map(_.capa), load, card))

    for (i <- 1 until cargos.size) {
      cp.add(new ChemicalConstraint(cargos(i), tanks, cargo)) // dominance rules
    }
    // enforce that for any two neighbor tanks, they must contain compatible cargo types
    for (t <- tanks; t2 <- t.neighbours; if (t2 > t.id)) {
      cp.add(table(cargo(t.id - 1), cargo(t2 - 1), compatibles))
    }

  } exploration {
    while (!allBounds(cargo)) {
      val volumeLeft = Array.tabulate(cargos.size)(c => cargos(c).volume - volumeAllocated(c))
      // the largest tank having still no cargo assigned to it
      //val (tankVar,tank) = cargo.zipWithIndex.filter(c => !c._1.isBound).maxBy(c => (tanks(c._2).capa,-c._1.getSize))
      val unboundTanks = cargo.zipWithIndex.filter { case (x, c) => !x.isBound }
      val (tankVar, tank) = unboundTanks.maxBy { case (x, c) => (tanks(c).capa, -x.getSize) }
      val cargoToPlace = (0 until cargos.size).filter(tankVar.hasValue(_)).maxBy(volumeLeft(_))
      cp.branch(cp.post(tankVar == cargoToPlace))(cp.post(tankVar != cargoToPlace))
    }
    println("solution")
    nbSol += 1
    for (i <- 0 until cargo.size) {
      cargosol(i) = cargo(i).getValue
    }
    val volumeLeft = Array.tabulate(cargos.size)(c => cargos(c).volume - volumeAllocated(c))
    println("total slack:" + (-(volumeLeft.sum - volumeLeft(0))) + " tanks capas:" + tanks.map(_.capa).mkString(","))
    cargos.zipWithIndex.foreach { case (c, i) => barChart.setValue("Slack", i.toString, -volumeLeft(i)) }
  }
}