package oscar.des.examples
import oscar.stochastic.Learning
import oscar.des.engine._
import oscar.invariants._
import org.scala_tools.time.Imports._
import scala.util.continuations._
import oscar.stochastic.Flip
import oscar.stochastic.UniformDiscrete
import oscar.stochastic.UniformChoice
import oscar.des.engine.examples.gui.EpidemyDisplay


object LaunchED extends App {
  EpidemyDisplay.main(Array())
}

/** 
 * Epidemic simulation inspired from https://class.coursera.org/reactive-001/assignment/view?assignment_id=7
 * Developed in OscaR
 * 
 * @author FX Mouthuy
 */
object EpidemySimulator  {
  object parameters {
    val nbPersons = 10
    val nbColumns = 3
    val nbRows    = 3
    val nbRealisations = 1
  }
  
  def main(args: Array[String]) = {
    val simu = new EpidemySimulation
    for (rea <- 0 until parameters.nbRealisations){
      simu.simulate()
      print(".")
    }
    
    println("%nnumber of dead in average after a year: %s" format simu.deadInAYear.mean(parameters.nbRealisations))
  }
}
class EpidemySimulation extends Learning {
  val deadInAYear = learnNumber[Double]()
  
  def simulate() : Unit = {
    /* start the simulation */
    implicit val m = new StochasticModel[Unit]
    m.setTime(DateTime.now)
    
    val village = new Village()
    
    whenever(village.graveyard) { p =>
      //println("%s is dead" format p.id)
      deadInAYear observe 1
    }
    
	m.simulate(DateTime.now.plusYears(1), false)
  }
}

class Village(
    val nbPersons: Int = EpidemySimulator.parameters.nbPersons, 
    nbRows: Int = EpidemySimulator.parameters.nbRows,
    nbColumns: Int = EpidemySimulator.parameters.nbColumns)
    (implicit m: Model[Unit]) extends Process[Unit]("the village")(m) {
  
  /* distribution for placement & moves */
  val random    = new scala.util.Random
  val getRow    = UniformDiscrete(0,nbRows-1)
  val getColumn = UniformDiscrete(0,nbColumns-1)
  val nbWaitingDays = UniformDiscrete(1,5).map(_.days)
  def getInitialPosition = Position(random nextInt nbRows,random nextInt nbColumns)
  def getPosition(m:Model[Unit]) = Position(getRow(m),getColumn(m))
  /* distribution for the disease */
  val dropDead    = Flip(0.25)
  val getInfected = Flip(0.40)
  val usePlane    = Flip(0.01)
  
  /* all dead people go there */
  val graveyard = Event[VillagePerson]
  
  /* other processes */
  val population = for (i <- 0 until nbPersons) yield VillagePerson(i,getInitialPosition,if (random.nextDouble < 0.1) Infected(0) else Healthy(0), this)
  

  override def start() = {
    
  }
  
  def left(col:Int) = (col + nbColumns - 1) % nbColumns
  def right(col:Int) = (col + 1) % nbColumns
  def up(row:Int) = (row + nbRows - 1) % nbRows
  def down(row:Int) = (row + 1) % nbRows
  
  val surroundingHouses:Function1[Position,List[Position]] = {
    case Position(row,col) => List(Position(up(row),col),Position(down(row),col),Position(row,left(col)),Position(row,right(col)))
  }
  def visiblySafeSurroundingHouses(position:Position): List[Position] = surroundingHouses(position).filter { p => 
    population.forall( v => v.moves.currentState()!= p || !v.health.visibilyInfectious())
  }
  def isSafe(position:Position) = population.forall( v => v.moves.currentState()!= position || !v.health.infectious())
}


/** person states: moves and sickness */
case class Position(val row:Int, val col:Int)
sealed trait HealthState{
  def step:Int
}
case class Healthy(step:Int) extends HealthState
case class Infected(step:Int) extends HealthState
case class Sick(step:Int) extends HealthState
case class Immune(step:Int) extends HealthState
case class Dead(step:Int) extends HealthState

/** a person is composed of two processes: health and position */
case class VillagePerson(val id:Int,val initialPosition:Position, val initialHealth:HealthState,village: Village)(implicit val m: Model[Unit])  {
  
  /** process of moving though all rooms */
  val moves: ProcessWithStates[Position,Unit] with SaveState[Position,Unit] = new ProcessWithStates[Position,Unit] ("Position of %s" format id,initialPosition) with SaveState[Position,Unit]  {
    override def exec(implicit state:Position) = {
      if (!village.isSafe(state)) health.getInfected else unit // coming into a room with a sick might make you sick 
      waitDuring(village.nbWaitingDays(m))
      if (!health.currentState().isInstanceOf[Dead]) {
        // select a nearby room with no visibly infectious person inside
        if (village.usePlane(m)) Iam(village.getPosition(m))
        else {
	      val options = village.visiblySafeSurroundingHouses(state)
	      if (options.isEmpty) Iam(state)
	      else Iam(UniformChoice(options)(m))
        }
      }
    }
  }
  
  /** process of the person health */
  val health = new ProcessWithStates[HealthState,Unit] ("Health of %s" format id,initialHealth) with SaveState[HealthState,Unit] /*with LogState[HealthState,Unit] */{
    val infectious = new Var[Boolean](false)
    val visibilyInfectious = new Var[Boolean](false)
    override def exec(implicit state:HealthState) = state match {
      case Healthy(step) =>
        infectious := false
        visibilyInfectious := false
      case Infected(step) =>
        infectious := true
        waitDuring(15.days)
        Iam(Sick(step+1))
      case Sick(step) => 
        visibilyInfectious := true
        waitDuring(8.days)
        if (village.dropDead(m))
          Iam(Dead(step+1))
        else {
          waitDuring(2.days)
          Iam(Immune(step+1))
        }
      case Immune(step) =>
        visibilyInfectious := false
        waitDuring(2.days)
        Iam(Healthy(step))
      case Dead(step) =>
        village.graveyard emit VillagePerson.this
    }
    
    def getInfected = {
      if (village.getInfected(m) && currentState().isInstanceOf[Healthy])
        Iam(Infected(currentState().step))(currentState())
    }
    
  }
  
  def isHealthy: Boolean = health.currentState().isInstanceOf[Healthy]
  def isImmune : Boolean = health.currentState().isInstanceOf[Immune]
  def isSick: Boolean = !isHealthy && !isImmune
  
  def isDead: Boolean = health.currentState().isInstanceOf[Healthy]
}