package oscar.cbls.search

import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.{CBLSIntVar, CBLSSetVar}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.algo.IdenticalAggregator
import oscar.cbls.search.core.{Neighborhood, NoMoveFound, SearchResult, StatelessNeighborhood}
import oscar.cbls.search.move.{AssignMove, CompositeMove, Move, SwapMove}

import scala.collection.immutable.SortedSet

/**
 * will find a variable in the array, and find a value from its range that improves the objective function
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param obj te objective function to improve
 * @param name the name of the neighborhood
 * @param best true for the best move, false for the first move, default false
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param symmetryClassOfVariables a function that input the ID of a variable and returns a symmetry class;
 *                      ony one of the variable in each class will be considered to make search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param symmetryClassOfValues a function that inputs the ID of a variable and a possible value for this variable,
 *                              and returns a symmetry class for this variable and value
 *                              only values belonging to different same symmetry classes will be tested
 *                             Int.MinValue is considered different to itself
 *                             (this is only useful if your model is awfully expensive to evaluate)
 */
case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              obj:Objective,
                              name:String = "AssignNeighborhood",
                              acceptanceCriteria:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj,
                              best:Boolean = false,
                              searchZone:CBLSSetVar = null,
                              symmetryClassOfVariables:Option[Int => Int] = None,
                              symmetryClassOfValues:Option[Int => Int => Int] = None)
  extends Neighborhood with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj): SearchResult = {
    if (amIVerbose) println(name + ": trying")
    var oldObj = obj.value
    var toReturn: SearchResult = NoMoveFound

    val iterationSchemeOnZone = if (searchZone == null)
      if (best) vars.indices
      else vars.indices startBy startIndice
    else searchZone.value

    val iterationScheme = symmetryClassOfVariables match {
      case None => iterationSchemeOnZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(iterationSchemeOnZone, s)
    }

    for (i <- iterationScheme) {
      val currentVar = vars(i)
      val oldVal = currentVar.value

      val domainIterationScheme = symmetryClassOfValues match {
        case None => currentVar.domain
        case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(currentVar.domain, s(i))
      }

      for (newVal <- domainIterationScheme if newVal != oldVal) {
        val newObj = obj.assignVal(currentVar, newVal)

        if (acceptanceCriteria(oldObj, newObj)) {
          if (best) {
            oldObj = newObj
            toReturn = AssignMove(currentVar, newVal, newObj, name)
          } else {
            if (searchZone == null) startIndice = i
            if (amIVerbose) println(name + ": move found")
            return AssignMove(currentVar, newVal, newObj, name)
          }
        }
      }
    }
    if(amIVerbose) {
      toReturn match {
        case NoMoveFound => println(name + ": no move found")
        case _ => println(name + ": move found")
      }
    }
    toReturn
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * will iteratively swap the value of two different variables in the array
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param obj te objective function to improve
 * @param searchZone1 a subset of the indices of vars to consider for the first moved point
 *                   If none is provided, all the array will be considered each time
 * @param searchZone2 a subset of the indices of vars to consider for the second moved point
 *                   If none is provided, all the array will be considered each time
 * @param symmetryCanBeBroken set to true, and the neighborhood will break symmetries on indices of swapped vars
 *                            typically, you always want it except if you have specified the two searchZones, and they are different
 * @param name the name of the neighborhood
 * @param symmetryClass a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered
 *                      this makes search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 **/
case class SwapsNeighborhood(vars:Array[CBLSIntVar],
                             obj:Objective,
                             name:String = "SwapsNeighborhood",
                             searchZone1:CBLSSetVar = null,
                             searchZone2:CBLSSetVar = null,
                             symmetryCanBeBroken:Boolean = true,
                             best:Boolean = false,
                             symmetryClass:Option[Int => Int] = None)
  extends Neighborhood with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj): SearchResult = {

    if(amIVerbose) println(name + ": trying")
    var oldObj = obj.value
    var toReturn:SearchResult = NoMoveFound

    val firstIterationSchemeZone = if(searchZone1 == null) if (best) vars.indices else vars.indices startBy startIndice else searchZone1.value

    val firstIterationScheme = symmetryClass match {
      case None => firstIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(firstIterationSchemeZone, s)
    }

    val secondIterationSchemeZone = if(searchZone2 == null) vars.indices else searchZone2.value

    val secondIterationScheme = symmetryClass match {
      case None => secondIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(secondIterationSchemeZone, s)
    }

    for(i:Int <- firstIterationScheme){
      val firstVar = vars(i)
      val oldValOfFirstVar = firstVar.value

      for(j:Int <- secondIterationScheme;
          secondVar = vars(j);
          oldValOfSecondVar = secondVar.value
          if (!symmetryCanBeBroken || i < j)  //we break symmetry here
            && i != j
            && oldValOfFirstVar != oldValOfSecondVar
            && secondVar.domain.contains(oldValOfFirstVar)
            && firstVar.domain.contains(oldValOfSecondVar)) {

        val newObj = obj.swapVal(firstVar, secondVar)
        if (acceptanceCriteria(oldObj,newObj)) {
          if(best){
            toReturn =  SwapMove(firstVar, secondVar, newObj, name)
            oldObj = newObj
          }else {
            if (searchZone1 == null) startIndice = i
            if (amIVerbose) println(name + ": move found")
            return SwapMove(firstVar, secondVar, newObj, name)
          }
        }
      }
    }
    if(amIVerbose) {
      toReturn match {
        case NoMoveFound => println(name + ": no move found")
        case _ => println(name + ": move found")
      }
    }
    toReturn
  }

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * will randomize the array, typically to get out of a local minimal
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param name the name of the neighborhood
 */
case class RandomizeNeighborhood(vars:Array[CBLSIntVar],
                                 degree:Int = 1,
                                 name:String = "RandomizeNeighborhood",
                                 searchZone:CBLSSetVar = null)
  extends StatelessNeighborhood with AlgebraTrait with SearchEngineTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = null): SearchResult = {
    if(amIVerbose) println("applying " + name)

    var toReturn:List[Move] = List.empty

    if(searchZone != null && searchZone.value.size <= degree){
      //We move everything
      for(i <- searchZone.value){
        toReturn = AssignMove(vars(i),selectFrom(vars(i).domain),0) :: toReturn
      }
    }else{
      var touchedVars:Set[Int] = SortedSet.empty
      for(r <- 1 to degree){
        val i = selectFrom(vars.indices,(j:Int) => (searchZone == null || searchZone.value.contains(j)) && !touchedVars.contains(j))
        touchedVars = touchedVars + i
        val oldVal = vars(i).value
        toReturn = AssignMove(vars(i),selectFrom(vars(i).domain, (_:Int) != oldVal),0) :: toReturn
      }
    }
    if(amIVerbose) println(name + ": move found")
    CompositeMove(toReturn, 0, name)
  }
}

/**
 *  will chose a variable in the array of variable that maximizes its violation (ties broken randomly)
 *  and find a value from its range that improves the objective function
 *  the new value can be either the best one or the first one that improves according to parameter "best"
 *
 *  notice that the search of variable is performed linearly, as for the search of new value.
 *  For a smarter search, one should use [[oscar.cbls.search.AssignNeighborhood]] and a searchZone set with [[oscar.cbls.invariants.lib.minmax.ArgMaxArray]]
 *
 * @param c the constraint system
 * @param variables the array of variable that define the search space of this neighborhood
 * @param best true: the new value is the best one, false, the new value is the first found one that improves
 */
class ConflictAssignNeighborhood(c:ConstraintSystem, variables:List[CBLSIntVar], best:Boolean = false) extends StatelessNeighborhood with SearchEngineTrait{
  var varArray = variables.toArray
  val violations:Array[CBLSIntVar] = varArray.clone().map(c.violation(_))
  override def getImprovingMove(acceptanceCriteria:(Int,Int) => Boolean = (oldObj,newObj) => oldObj > newObj): SearchResult = {
    val oldObj = c.ObjectiveVar.value
    val MaxViolVarID = selectMax(varArray.indices,violations(_:Int).value)

    val NewVal = if(best) selectMin(varArray(MaxViolVarID).domain)(c.assignVal(varArray(MaxViolVarID),_:Int))
    else selectFirst(varArray(MaxViolVarID).domain, (newVal:Int) => c.assignVal(varArray(MaxViolVarID),newVal) < oldObj)
    val newObj = c.assignVal(varArray(MaxViolVarID),NewVal)

    if(acceptanceCriteria(oldObj,newObj)){
      AssignMove(varArray(MaxViolVarID),NewVal,newObj)
    }else{
      NoMoveFound
    }
  }
}

