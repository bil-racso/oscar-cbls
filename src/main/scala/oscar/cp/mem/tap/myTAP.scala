package oscar.cp.mem.tap

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import oscar.reversible._
import oscar.visual._
import scala.collection.JavaConversions._
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
 * @author Pierre Schaus pschaus@gmail.com
 */
object MyTAP extends App {

  // Data parsing
  // ------------------------------------------
  
  val instance = TAPUtils.parseInstance("data/chemical2.xml")  
  val cargos = instance.cargos
  val tanks = instance.tanks
  val totCapa = instance.totCapa
  val incompatibles = instance.incompatibles
  val compatibles = instance.compatibles
    

  // Visualization
  // ------------------------------------------
  
  val f = new VisualFrame("ChemicalTanker")
  // creates the plot and place it into the frame
  val plot = new Plot2D("", "Solution number", "Unused Volume")
  f.createFrame("Objective Function: Unused Volume").add(plot)
  // creates the tour visu and place it into the frame
  val drawing = new VisualDrawing(false)
  f.createFrame("Cargo-Tank Layout").add(drawing)
  f.pack()
  // create the bar chart with the volume to load and volume slacks		            
  val barChart = new BarChart("", "Cargos", "Volume", Array("Volume", "Slack"), (0 until cargos.size).map("" + _).toArray, true)
  cargos.zipWithIndex.foreach { case (c, i) => barChart.setValue("Volume", i.toString, c.volume) }
  f.createFrame("Volume Slack").add(barChart)
  val rects = Array.tabulate(tanks.size)(t => {
    new VisualRectangle(drawing, 100 + tanks(t).y * 30, 50 + tanks(t).x * 30, tanks(t).h * 30, tanks(t).w * 30)
  })
  def setCargo(t: Int, c: Cargo) = {
    rects(t).innerCol = c.color
    rects(t).toolTip = "<html>" + c.name + "<br>capa:" + tanks(t).capa + "<html>"
  }


  // Model
  // ------------------------------------------

  val cp = CPSolver()
  
  // for each tank, the cargo type placed into it (dummy cargo if empty)
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

  // LNS
  // ------------------------------------------

  case class Sol(cargo: Array[Int], freeSpace: Int, nbFreeTanks: Int)
  var currentSol: Sol = null
  val p = 30

  cp.lns(1000, 500) { relaxVariables(randomRelax(p)) }

  def randomRelax(p: Int): Array[Boolean] = {
    Array.tabulate(cargos.size)(i => rnd.nextInt(100) > p && !cp.isFailed())
  }

  def relaxVariables(selected: Array[Boolean]) {
    val constraints: Queue[Constraint] = Queue()
    for (i <- 0 until cargos.size; if selected(i)) {
      cp.post(cargo(i) == currentSol.cargo(i))
    }
    cp.post(constraints.toArray)
  }

  def solFound() {
    currentSol = Sol(cargo.map(_.value), freeSpace.value, nbFreeTanks.value)
  }

  
  // Search + Constraints
  // ------------------------------------------
  
  var nbSol = 0
  val slack = Array.tabulate(cargos.size)(c => load(0) - cargos(c).volume)

  cp.minimize(if (true) -freeSpace else -nbFreeTanks) 
  
  cp.subjectTo {
    // make the link between cargo and load vars with binpacking constraint
    cp.add(binpacking(cargo, tanks.map(_.capa), load), Strong)
    cp.add(binpackingCardinality(cargo, tanks.map(_.capa), load, card))

    // dominance rules
    for (i <- 1 until cargos.size)
      cp.add(new ChemicalConstraint(cargos(i), tanks, cargo)) 

    // enforce that for any two neighbor tanks, they must contain compatible cargo types
    for (t <- tanks; t2 <- t.neighbours; if (t2 > t.id))
      cp.add(table(cargo(t.id - 1), cargo(t2 - 1), compatibles))
  } 
  
  cp.exploration {
    
    while (!allBounds(cargo)) {     
      val volumeLeft = Array.tabulate(cargos.size)(c => cargos(c).volume - volumeAllocated(c))
      val unboundTanks = cargo.zipWithIndex.filter { case (x, c) => !x.isBound }
      
      val (tankVar, tank) = unboundTanks.maxBy { case (x, c) => (tanks(c).capa, -x.getSize) }
      //val (tankVar,tank) = cargo.zipWithIndex.filter(c => !c._1.isBound).maxBy(c => (tanks(c._2).capa,-c._1.getSize))
      
      val cargoToPlace = (0 until cargos.size).filter(tankVar.hasValue(_)).maxBy(volumeLeft(_))
      
      cp.branch(cp.post(tankVar == cargoToPlace))(cp.post(tankVar != cargoToPlace))
    }

    println("solution")
    solFound()
    println(currentSol.freeSpace + " " + currentSol.nbFreeTanks)
    nbSol += 1

    // updates visualizations
    for (i <- 0 until cargo.size) {
      setCargo(i, cargos(cargo(i).value))
    }
    val volumeLeft = Array.tabulate(cargos.size)(c => cargos(c).volume - volumeAllocated(c))
    cargos.zipWithIndex.foreach { case (c, i) => barChart.setValue("Slack", i.toString, -volumeLeft(i)) }
    plot.addPoint(nbSol, freeSpace.value)
  }

}