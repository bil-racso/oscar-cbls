package oscar.cbls.search

import oscar.cbls.invariants.core.computation.{CBLSSetVar, CBLSIntVar}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.moves._

//TODO: symmetry elimination static & dynamic
//TODO: add some random stuff

/**
 * will iteratively find a variable in the array, and find a value from its range that improves the objective function
 *
 * @param vars an array of [[CBLSIntVar]] defining the search space
 * @param obj te objective function to improve
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param name the name of the neighborhood
 */
case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              obj:Objective,
                              name:String = "IntAdjust",
                              searchZone:CBLSSetVar = null,
                              solvedPivot:Int = Int.MinValue)
  extends Neighborhood with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def getImprovingMove: SearchResult = {

    val oldObj = obj.value
    if(oldObj <= solvedPivot) return ProblemSolved

    val iterationScheme = if(searchZone == null) vars.indices startBy startIndice else searchZone.value

    for(i <- iterationScheme){
      val currentVar = vars(i)
      val oldVal = currentVar.value

      for(newVal <- currentVar.domain if newVal != oldVal){
        val newObj = obj.assignVal(currentVar,newVal)

        if(newObj < oldObj){
          if(searchZone == null) startIndice = i
          return AssignMove(currentVar,newVal, newObj, name)
        }
      }
    }
    NoMoveFound
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * will iteratively swap the value of two different variables in the array
 *
 * @param vars an array of [[CBLSIntVar]] defining the search space
 * @param obj te objective function to improve
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param name the name of the neighborhood
 */
case class SwapsNeighborhood(vars:Array[CBLSIntVar], obj:Objective, name:String = "IntSwaps", searchZone:CBLSSetVar = null, solvedPivot:Int = Int.MinValue)
  extends Neighborhood with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def getImprovingMove: SearchResult = {

    val oldObj = obj.value

    if(oldObj <= solvedPivot) return ProblemSolved

    val firstIterationScheme = if(searchZone == null) vars.indices startBy startIndice else searchZone.value
    val secondIterationScheme = if(searchZone == null) vars.indices else searchZone.value

    for(i:Int <- firstIterationScheme){
      val firstVar = vars(i)
      val oldValOfFirstVar = firstVar.value

      for(j:Int <- secondIterationScheme;
        secondVar = vars(j);
        oldValOfSecondVar = secondVar.value;
        if i < j  //we break symmetry here
        && oldValOfFirstVar != oldValOfSecondVar
        && secondVar.domain.contains(oldValOfFirstVar)
        && firstVar.domain.contains(oldValOfSecondVar)) {

        val newObj = obj.swapVal(firstVar, secondVar)
        if (newObj < oldObj) {
          if (searchZone == null) startIndice = i
          return SwapMove(firstVar, secondVar, newObj, name)
        }
      }
    }
    NoMoveFound
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

