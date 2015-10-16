package oscar.cbls.search

import oscar.cbls.invariants.core.computation.CBLSIntVar
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.{LoggingObjective, Objective}
import oscar.cbls.search.algo.{HotRestart, IdenticalAggregator}
import oscar.cbls.search.core.{Neighborhood, NoMoveFound, SearchResult}
import oscar.cbls.search.move.{AssignMove, Move}


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

trait moveInstanciator[MyMove <: Move] {
  def instantiateMove(newObj: Int): MyMove
}

//TODO: un obj peut être un moveInstanciator en fait, suffit de sauver les valeurs du mouvement
// (si disponible, ce qui n'est pas toujours le cas.)

