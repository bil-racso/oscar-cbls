package oscar.cbls.search

import oscar.cbls.invariants.core.computation.{CBLSIntVar, Solution, Store, Variable}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.{LoggingObjective, Objective}
import oscar.cbls.search.algo.{HotRestart, IdenticalAggregator}
import oscar.cbls.search.combinators.NeighborhoodCombinator
import oscar.cbls.search.core.{MoveFound, Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.search.move.{AssignMove, CompositeMove, Move}

abstract class EasyNeighborhood2(best:Boolean = false, neighborhoodName:String=null)
  extends Neighborhood with SupportForAndThenChaining{

  protected def neighborhoodNameToString: String = if (neighborhoodName != null) neighborhoodName else this.getClass().getSimpleName()

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

  def instantiateCurrentMove(newObj: Int): Move

  var tmpNewObj: Int = 0

  def evaluateCurrentMoveObjTrueIfStopRequired(newObj: Int): Boolean = {
    //cas à gérer:
    //verbose (on affiche le mouvement; qu'on instancie donc avec obj)
    //andThen (pas utile de l'instancier sns obj pq on va de tt façons propager en commençant le voisinage suivant.
    //normal
    //pour l'instant, cas normal uniquement (en fait le AndThen sera géré par le combinateur idoine)

    val myPrintExploredNeighbors = printExploredNeighbors

    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = instantiateCurrentMove(newObj)
        if (myPrintExploredNeighbors) {
          println("Explored " + toReturnMove + ", new best, saved (might be filtered out if best is not accepted)")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      } else {
        if (myPrintExploredNeighbors) {
          println("Explored " + instantiateCurrentMove(newObj) + ", not the new best, not saved")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
      }
      false //since we are looking for the best one, we do not stop
    } else {
      if (acceptanceCriterion(oldObj, newObj)) {
        bestNewObj = newObj
        toReturnMove = instantiateCurrentMove(newObj)
        if (myPrintExploredNeighbors) {
          println("Explored " + toReturnMove + ", accepted, exploration stopped")
          println(obj.asInstanceOf[LoggingObjective].getAndCleanEvaluationLog.mkString("\n"))
        }
        true //since we are looking for the first one, we stop
      } else {
        //explored, but not saved
        if (myPrintExploredNeighbors) {
          println("Explored " + instantiateCurrentMove(newObj) + ", not accepted, not saved")
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
  extends EasyNeighborhood2(best,name) with AlgebraTrait{
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
          val newObj = obj.assignVal(currentVar,newVal)
          if (evaluateCurrentMoveObjTrueIfStopRequired(newObj)){
            startIndice = currentIndice + 1
            return
          }
        }
      }
    }
  }

  override def instantiateCurrentMove(newObj:Int) =
    AssignMove(currentVar, newVal, newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

trait SupportForAndThenChaining extends Neighborhood{

  def instantiateCurrentMove(newObj:Int):Move

  def dynAndThen(other:Move => Neighborhood,maximalIntermediaryDegradation: Int = Int.MaxValue):DynAndThen = {
    DynAndThen(this,other,maximalIntermediaryDegradation)
  }
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

case class DynAndThen(a:Neighborhood with SupportForAndThenChaining,
                      b:(Move => Neighborhood),
                      maximalIntermediaryDegradation: Int = Int.MaxValue)
  extends NeighborhoodCombinator(a) with SupportForAndThenChaining{

  var currentB:Neighborhood = null

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

    class InstrumentedObjective() extends Objective{

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
        currentB = b(a.instantiateCurrentMove(intermediaryObjValue))

        currentB.getMove(obj, secondAcceptanceCriteria) match {
          case NoMoveFound => Int.MaxValue
          case MoveFound(m: Move) =>
            secondMove = m
            m.objAfter
        }
      }
    }

    val tmp = a.getMove(new InstrumentedObjective(), firstAcceptanceCriterion)

     tmp match {
      case NoMoveFound => NoMoveFound
      case MoveFound(m: Move) => CompositeMove(List(m, secondMove), m.objAfter, this.toString)
    }
  }


  override def instantiateCurrentMove(newObj: Int): Move ={
    currentB match{
      case null => throw new Error("DynAndThen is not presently exploring something")
      case s:SupportForAndThenChaining =>
        CompositeMove(List(a.instantiateCurrentMove(Int.MaxValue),
          s.instantiateCurrentMove(Int.MaxValue)),newObj,"DynAndThen(" + a + "," + currentB + ")")
      case _ => throw new Error("DynAndThen: Neighborhood on the right cannot be chained")
    }
  }
}

case class DynAndThenWithPrev(x:Neighborhood with SupportForAndThenChaining,
                              b:((Move,Solution) => Neighborhood),
                              maximalIntermediaryDegradation:Int = Int.MaxValue,
                              decisionVariablesToSave:Store => Iterable[Variable] = (s:Store) => s.decisionVariables()) extends NeighborhoodCombinator(x){

  val instrumentedA = new SaveDecisionVarsOnEntry(x,decisionVariablesToSave) with SupportForAndThenChaining{
    override def instantiateCurrentMove(newObj: Int): Move = x.instantiateCurrentMove(newObj)
  }

  val slave = DynAndThen(instrumentedA,
    (m:Move) => b(m,instrumentedA.savedSolution),
    maximalIntermediaryDegradation)

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean): SearchResult = slave.getMove(obj,acceptanceCriterion)
}

case class SaveDecisionVarsOnEntry(a: Neighborhood, decisionVariablesToSave:Store => Iterable[Variable] = (s:Store) => s.decisionVariables())
  extends NeighborhoodCombinator(a){

  var savedSolution:Solution = null

  override def getMove(obj: Objective, acceptanceCriterion: (Int, Int) => Boolean = (oldObj, newObj) => oldObj > newObj): SearchResult = {
    val s = obj.model
    savedSolution = s.saveValues(decisionVariablesToSave(s))
    a.getMove(obj,acceptanceCriterion)
  }
}

