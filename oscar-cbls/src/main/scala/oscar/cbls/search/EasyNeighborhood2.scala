package oscar.cbls.search

import oscar.cbls.invariants.core.computation.{Solution, Store, Variable, CBLSIntVar}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.{LoggingObjective, Objective}
import oscar.cbls.search.algo.{HotRestart, IdenticalAggregator}
import oscar.cbls.search.combinators.NeighborhoodCombinator
import oscar.cbls.search.core.{MoveFound, Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.search.move.{CompositeMove, CallBackMove, AssignMove, Move}


abstract class EasyNeighborhood2[MyMove <: Move](best:Boolean = false, neighborhoodName:String=null) extends Neighborhood {

  protected def neighborhoodNameToString: String = if (neighborhoodName != null) neighborhoodName else this.getClass.getSimpleName()

  override def toString: String = neighborhoodNameToString

  //passing parameters, and getting return values from the search
  private var oldObj: Int = 0
  private var acceptanceCriterion: (Int, Int) => Boolean = null
  private var toReturnMove: Move = null
  private var bestNewObj: Int = Int.MaxValue
  protected var obj: Objective = null

  override final def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = {
    oldObj = obj()
    this.acceptanceCriterion = acceptanceCriterion
    toReturnMove = null
    bestNewObj = Int.MaxValue
    this.obj = if (printExploredNeighbors) new LoggingObjective(obj) else obj
    if (printPerformedSearches) println(neighborhoodNameToString + ": start exploration")

    exploreNeighborhood()

    if (toReturnMove == null || (best && !acceptanceCriterion(oldObj, bestNewObj))) {
      if (printPerformedSearches) {
        println(neighborhoodNameToString + ": no move found")
      }
      NoMoveFound
    } else {
      if (printPerformedSearches) {
        println(neighborhoodNameToString + ": move found: " + toReturnMove)
      }
      toReturnMove
    }
  }

  /** This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  def exploreNeighborhood()

  def evaluateObj(): Int

  def instantiateMove(newObj: Int): MyMove

  var tmpNewObj: Int = 0

  def exploreMoveTrueIfStopRequired(): Boolean = {
    //cas à gérer:
    //verbose (on affiche le mouvement; qu'on instancie donc avec obj)
    //andThen (pas utile de l'instancier sns obj pq on va de tt façons propager en commençant le voisinage suivant.
    //normal
    //pour l'instant, cas normal uniquement (en fait le AndThen sera géré par le combinateur idoine)

    val newObj = evaluateObj()

    val myPrintExploredNeighbors = printExploredNeighbors

    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = instantiateMove(newObj)
        if (myPrintExploredNeighbors) {
          println("Explored " + toReturnMove + ", new best, saved (might be filtered out if best is not accepted)")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      } else {
        if (myPrintExploredNeighbors) {
          println("Explored " + instantiateMove(newObj) + ", not the new best, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      }
      false //since we are looking for the best one, we do not stop
    } else {
      if (acceptanceCriterion(oldObj, newObj)) {
        bestNewObj = newObj
        toReturnMove = instantiateMove(newObj)
        if (myPrintExploredNeighbors) {
          println("Explored " + toReturnMove + ", accepted, exploration stopped")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        true //since we are looking for the first one, we stop
      } else {
        //explored, but not saved
        if (myPrintExploredNeighbors) {
          println("Explored " + instantiateMove(newObj) + ", not accepted, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        false
      }
    }
  }
}

case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              name:String = "AssignNeighborhood",
                              best:Boolean = false,
                              searchZone:() => Iterable[Int] = null,
                              symmetryClassOfVariables:Option[Int => Int] = None,
                              symmetryClassOfValues:Option[Int => Int => Int] = None,
                              domain:(CBLSIntVar,Int) => Iterable[Int] = (v,i) => v.domain,
                              hotRestart:Boolean = true)
  extends EasyNeighborhood2[AssignMove](best,name) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0

  var currentVar:CBLSIntVar = null
  var newVal:Int = 0

  override def exploreNeighborhood() {

    val iterationZone =
      if (searchZone == null) vars.indices
      else searchZone()

    val iterationSchemeOnZone =
      if (hotRestart && !best) HotRestart(iterationZone, startIndice)
      else iterationZone

    val iterationSchemeOnSymmetryFreeZone = symmetryClassOfVariables match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, (index:Int) => (s(index),vars(index).value))
    }

    //iterating over the variables to consider
    val indicesIterator = iterationSchemeOnSymmetryFreeZone.iterator
    while(indicesIterator.hasNext){
      val currentIndice = indicesIterator.next()
      currentVar = vars(currentIndice)
      //now we have the current variable

      val oldVal = currentVar.value

      val domainIterationScheme = symmetryClassOfValues match {
        case None => domain(currentVar, currentIndice)
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(domain(currentVar, currentIndice), s(currentIndice))
      }

      //iterating over the values of the variable
      val domainIterationSchemeIterator = domainIterationScheme.iterator
      while(domainIterationSchemeIterator.hasNext){
        newVal = domainIterationSchemeIterator.next()
        if (newVal != oldVal){
          //testing newValue
          if (exploreMoveTrueIfStopRequired()){
            startIndice = currentIndice + 1
            return
          }
        }
      }
    }
  }

  override def evaluateObj():Int =
    obj.assignVal(currentVar,newVal)

  override def instantiateMove(newObj:Int) =
    AssignMove(currentVar, newVal, newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

trait MoveInstanciator[MyMove <: Move] {
  def instantiateMove(newObj: Int): MyMove
}


trait MoveInstantiatorCarrier[MyMove <: Move]{
  var myMoveInstantiator:MoveInstanciator[MyMove] = null
  def setMoveInstantiator(m:MoveInstanciator[MyMove]): Unit ={
    myMoveInstantiator = m
  }

  def currentMove(newObj:Int = Int.MaxValue):MyMove = myMoveInstantiator.instantiateMove(newObj)
}

class AndThen(a: Neighborhood, b: Neighborhood, maximalIntermediaryDegradation: Int = Int.MaxValue)
  extends NeighborhoodCombinator(a, b) {

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    var secondMove: Move = null //the move performed by b
    val oldObj: Int = obj.value

    //the acceptance criterion is on the diff between the oldObj and the newObj over the two consecutive moves
    //it is evaluated for the second move
    //the first move is about accepting all moves that are not maxVal, since the newObj is for the consecutive moves,
    // and is already accepted by the time it is returned to the first neighrhood
    def firstAcceptanceCriterion(oldObj: Int, newObj: Int): Boolean = {
      newObj != Int.MaxValue
    }

    def secondAcceptanceCriteria(intermediaryObj: Int, newObj: Int): Boolean = {
      //we ignore the intermediaryObj.
      acceptanceCriteria(oldObj, newObj)
    }

    class InstrumentedObjective() extends Objective {

      override def detailedString(short: Boolean, indent: Int = 0): String = nSpace(indent) + "AndThenInstrumentedObjective(initialObjective:" + obj.detailedString(short) + ")"

      override def model = obj.model

      override def value: Int = {

        if (maximalIntermediaryDegradation != Int.MaxValue) {
          //we need to ensure that intermediary step is admissible
          val intermediaryVal = obj.value
          val intermediaryDegradation = intermediaryVal - oldObj
          if (intermediaryDegradation > maximalIntermediaryDegradation)
            return Int.MaxValue //we do not consider this first step
        }

        //now, we need to check the other neighborhood
        b.getMove(obj, secondAcceptanceCriteria) match {
          case NoMoveFound => Int.MaxValue
          case MoveFound(m: Move) =>
            secondMove = m
            m.objAfter
        }
      }
    }

    a.getMove(new InstrumentedObjective(), firstAcceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m: Move) => CompositeMove(List(m, secondMove), m.objAfter, this.toString)
    }
  }
}

//TODO: un obj peut être un moveInstanciator en fait, suffit de sauver les valeurs du mouvement
// (si disponible, ce qui n'est pas toujours le cas.)

case class DynAndThen[MyMove <: Move](a:EasyNeighborhood2[MyMove],
                                      b:(MyMove => Neighborhood),
                                      maximalIntermediaryDegradation: Int = Int.MaxValue)
  extends NeighborhoodCombinator(a) { //TODO: b is not represented, but reset is not needed, restriction: no profiling on b

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean): SearchResult = {

    var secondMove: Move = null //the move performed by b
    val oldObj: Int = obj.value

    //the acceptance criterion is on the diff between the oldObj and the newObj over the two consecutive moves
    //it is evaluated for the second move
    //the first move is about accepting all moves that are not maxVal, since the newObj is for the consecutive moves,
    // and is already accepted by the time it is returned to the first neighrhood
    def firstAcceptanceCriterion(oldObj: Int, newObj: Int): Boolean = {
      newObj != Int.MaxValue
    }

    def secondAcceptanceCriteria(intermediaryObj: Int, newObj: Int): Boolean = {
      //we ignore the intermediaryObj.
      acceptanceCriteria(oldObj, newObj)
    }

    class InstrumentedObjective() extends Objective with MoveInstantiatorCarrier[MyMove]{

      override def detailedString(short: Boolean, indent: Int = 0): String = nSpace(indent) + "AndThenInstrumentedObjective(initialObjective:" + obj.detailedString(short) + ")"

      override def model = obj.model

      override def value: Int = {

        val intermediaryObjValue =
        if (maximalIntermediaryDegradation != Int.MaxValue) {
          //we need to ensure that intermediary step is admissible
          val intermediaryVal = obj.value
          val intermediaryDegradation = intermediaryVal - oldObj
          if (intermediaryDegradation > maximalIntermediaryDegradation) {
            return Int.MaxValue //we do not consider this first step
          }else{
            intermediaryVal
          }
        }else{
          Int.MaxValue
        }

        //now, we need to check the other neighborhood
        //first, let's instantiate it:
        val otherNeighborhood = b(currentMove(intermediaryObjValue))
        otherNeighborhood.getMove(obj, secondAcceptanceCriteria) match {
          case NoMoveFound => Int.MaxValue
          case MoveFound(m: Move) =>
            secondMove = m
            m.objAfter
        }
      }
    }

    a.getMove(new InstrumentedObjective(), firstAcceptanceCriterion) match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m: Move) => CompositeMove(List(m, secondMove), m.objAfter, this.toString)
    }
  }
}

case class DynAndThenWithPrev[MyMove <: Move](a:EasyNeighborhood2[MyMove],
                                              b:((MyMove,Solution) => Neighborhood),
                                              decisionVariablesToSave:Store => Iterable[Variable] = (s:Store) => s.decisionVariables()){

}

case class SaveDecisionVarsOnEntry(a: Neighborhood, decisionVariablesToSave:Store => Iterable[Variable] = (s:Store) => s.decisionVariables())
  extends NeighborhoodCombinator(a) {

  var savedSolution:Solution = null

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
    val s = obj.model
    savedSolution = s.saveValues(decisionVariablesToSave(s))
    a.getMove(obj,acceptanceCriterion)
  }
}

