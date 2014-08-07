package oscar.cbls.search

import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.algo.{IdenticalAggregator, HotRestart}
import oscar.cbls.search.core.{NoMoveFound, SearchResult, Neighborhood}
import oscar.cbls.search.move.{AssignMove, Move}


abstract class EasyNeighborhood(best:Boolean = false, obj:()=>Int, neighborhoodName:String=null) extends Neighborhood{
  private var oldObj:Int=0
  private var acceptanceCriterion:(Int,Int) => Boolean=null
  private var toReturnMove:Move = null
  private var bestNewObj:Int = Int.MaxValue

  final def getImprovingMove(acceptanceCriterion:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj):SearchResult = {
    oldObj = obj()
    this.acceptanceCriterion = acceptanceCriterion

    searchImprovingMoveEasy()

    if(toReturnMove == null || (best && !acceptanceCriterion(oldObj,bestNewObj))) {
      if (amIVerbose) println(neighborhoodName + ": no move found")
      NoMoveFound
    }else{
      if (amIVerbose) println(neighborhoodName + ": move found")
      toReturnMove
    }
  }

  def searchImprovingMoveEasy()

  /**
   * @param newObj
   * @param m
   * @return true if the search must be stopped right now
   */
  def notifyMoveExplored(newObj:Int, m: =>Move):Boolean = {

    if (best) {
      if (newObj < bestNewObj) {
        bestNewObj = newObj
        toReturnMove = m
      }
    } else if (acceptanceCriterion(oldObj, newObj)) {
      toReturnMove = m
      if(amIVerbose) println(neighborhoodName + ": move found")
      return true
    }
    false
  }


  /**
   * @param newObj
   * @return true if the move is requested, then you should call submitFoundMove
   */
  def moveRequested(newObj:Int):Boolean = {
    if (best) {
      if (newObj < bestNewObj) {
        return true
      }
    } else if (acceptanceCriterion(oldObj, newObj)) {
      return true
    }
    false
  }

  /** you can only, and must call this method when you called moveRequested and it returned true
   * @param m the move. notice that the obj must be accurate
   * @return true if the search must be stopped right now
   */
  def submitFoundMove(m:Move):Boolean = {

    if (best) {
      if (m.objAfter < bestNewObj) {
        bestNewObj = m.objAfter
        toReturnMove = m
      }
    } else if (acceptanceCriterion(oldObj, m.objAfter)) {
      toReturnMove = m
      if(amIVerbose) println(neighborhoodName + ": move found")
      return true
    }
    false
  }

//  if(moveRequested(newObj))
//    if submitFoundMove(AssignMove(currentVar, newVal, newObj, name)) return
}

case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              obj:Objective,
                              name:String = "AssignNeighborhood",
                              best:Boolean = false,
                              searchZone:CBLSSetVar = null,
                              symmetryClassOfVariables:Option[Int => Int] = None,
                              symmetryClassOfValues:Option[Int => Int => Int] = None,
                              domain:(CBLSIntVar,Int) => Iterable[Int] = (v,i) => v.domain,
                              hotRestart:Boolean = true)
  extends EasyNeighborhood(best,obj) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0

  override def searchImprovingMoveEasy() {
    if (amIVerbose) println(name + ": trying")

    val iterationSchemeOnZone =
      if (searchZone == null) {
        if (hotRestart && !best) {
          if (startIndice >= vars.size) startIndice = 0
          vars.indices startBy startIndice
        }else vars.indices
      }else if (hotRestart && !best) HotRestart(searchZone.value, startIndice)
      else searchZone.value

    val iterationScheme = symmetryClassOfVariables match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, (index:Int) => (s(index),vars(index).value))
    }

    for (i <- iterationScheme) {
      startIndice = i + 1
      val currentVar = vars(i)
      val oldVal = currentVar.value
      val domainIterationScheme = symmetryClassOfValues match {
        case None => domain(currentVar, i)
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(domain(currentVar, i), s(i))
      }

      if (amIVerbose) println(name + ": exploring (best:" + best + ") " + currentVar + " values:" + domainIterationScheme)

      for (newVal <- domainIterationScheme if newVal != oldVal) {
        val newObj = obj.assignVal(currentVar, newVal)

        if (moveRequested(newObj))
          if (submitFoundMove(AssignMove(currentVar, newVal, newObj, name))) return

      }
    }
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}
