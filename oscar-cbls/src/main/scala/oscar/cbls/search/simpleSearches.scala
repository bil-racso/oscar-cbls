package oscar.cbls.search

import oscar.cbls.constraints.core.ConstraintSystem
import oscar.cbls.invariants.core.computation.{InvariantHelper, CBLSIntVar, CBLSSetVar, IntValue}
import oscar.cbls.modeling.AlgebraTrait
import oscar.cbls.objective.Objective
import oscar.cbls.search.algo.{HotRestart, IdenticalAggregator}
import oscar.cbls.search.core._
import oscar.cbls.search.move._

import scala.collection.immutable.SortedSet
import scala.util.Random

/**
 * will find a variable in the array, and find a value from its range that improves the objective function
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param name the name of the neighborhood
 * @param best true for the best move, false for the first move, default false
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param symmetryClassOfVariables a function that input the ID of a variable and returns a symmetry class;
 *                      ony one of the variable in each class will be considered to make search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 *                      variables of the same class with different values will not be considered as symmetrical
 * @param symmetryClassOfValues a function that inputs the ID of a variable and a possible value for this variable,
 *                              and returns a symmetry class for this variable and value
 *                              only values belonging to different symmetry classes will be tested
 *                             Int.MinValue is considered different to itself
 *                             (this is only useful if your model is awfully expensive to evaluate)
 * @param domain a function that receives a variable and its Id in the vars array
 *               and returns the domain that is searched for the variable
 *               by default, the domain of the variable is explored
 * @param hotRestart  if true, the exploration order in case you ar not going for the best is a hotRestart
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 */
case class AssignNeighborhood(vars:Array[CBLSIntVar],
                              name:String = "AssignNeighborhood",
                              best:Boolean = false,
                              searchZone:() => Iterable[Int] = null,
                              symmetryClassOfVariables:Option[Int => Int] = None,
                              symmetryClassOfValues:Option[Int => Int => Int] = None,
                              domain:(CBLSIntVar,Int) => Iterable[Int] = (v,i) => v.domain,
                              hotRestart:Boolean = true)
  extends EasyNeighborhood[AssignMove](best,name) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0

  var currentVar:CBLSIntVar = null
  var currentIndice:Int = 0
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
      currentIndice = indicesIterator.next()
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
    AssignMove(currentVar, newVal, currentIndice, newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * this neighborhood consider swap moves that swap the value of two CBLSIntVar
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param searchZone1 a subset of the indices of vars to consider for the first moved point
 *                   If none is provided, all the array will be considered each time
 * @param searchZone2 a subset of the indices of vars to consider for the second moved point
 *                   If none is provided, all the array will be considered each time
 * @param symmetryCanBeBrokenOnIndices if set to true, the neighborhood will break symmetries on indices of swapped vars
 *                            that is: thee first variable will always have an indice strictly smaller than the second swapped variable
 *                            typically, you always want it except if you have specified one or two searchZones, and they are different
 * @param symmetryCanBeBrokenOnValue if set to true, the neighborhood will break symmetries on values of swapped vars
 *                            that is: thee first variable will always have a value strictly smaller than the value of second swapped variable
 *                            you do not want to have both symmetryCanBeBrokenOnIndices and symmetryCanBeBrokenOnValue
 * @param name the name of the neighborhood
 * @param symmetryClassOfVariables1 a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone1
 *                      this makes search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param symmetryClassOfVariables2 a function that input the ID of a variable and returns a symmetry class;
 *                      for each role of the move, ony one of the variable in each class will be considered for the vars in searchZone2
 *                      this makes search faster
 *                      Int.MinValue is considered different to itself
 *                      if you set to None this will not be used at all
 * @param hotRestart  if true, the exploration order in case you ar not going for the best
 *                    is a hotRestart for the first swapped variable
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 **/
case class SwapsNeighborhood(vars:Array[CBLSIntVar],
                             name:String = "SwapsNeighborhood",
                             searchZone1:()=>Iterable[Int] = null,
                             searchZone2:()=>Iterable[Int] = null,
                             symmetryCanBeBrokenOnIndices:Boolean = true,
                             symmetryCanBeBrokenOnValue:Boolean = false,
                             best:Boolean = false,
                             symmetryClassOfVariables1:Option[Int => Int] = None,
                             symmetryClassOfVariables2:Option[Int => Int] = None,
                             hotRestart:Boolean = true)
  extends EasyNeighborhood[SwapMove](best,name) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def exploreNeighborhood() {

    val firstIterationSchemeZone =
      if (searchZone1 == null) {
        if (hotRestart && !best) {
          if (startIndice >= vars.size) startIndice = 0
          vars.indices startBy startIndice
        } else vars.indices
      } else if (hotRestart && !best) HotRestart(searchZone1(), startIndice) else searchZone1()

    val firstIterationScheme = symmetryClassOfVariables1 match {
      case None => firstIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(firstIterationSchemeZone, s)
    }

    val secondIterationSchemeZone = if (searchZone2 == null) vars.indices else searchZone2()

    val secondIterationScheme = symmetryClassOfVariables2 match {
      case None => secondIterationSchemeZone
      case Some(s) => IdenticalAggregator.removeIdenticalClassesLazily(secondIterationSchemeZone, s)
    }

    val iIterator = firstIterationScheme.iterator
    while (iIterator.hasNext) {
      val i = iIterator.next()
      val firstVar = vars(i)
      val oldValOfFirstVar = firstVar.value

      val jIterator = secondIterationScheme.iterator
      while (jIterator.hasNext) {
        val j = jIterator.next()
        val secondVar = vars(j)
        val oldValOfSecondVar = secondVar.value
        if ((!symmetryCanBeBrokenOnIndices || i < j) //we break symmetry on variables
          && i != j
          && (!symmetryCanBeBrokenOnValue || oldValOfFirstVar < oldValOfSecondVar) //we break symmetry on values
          && oldValOfFirstVar != oldValOfSecondVar
          && secondVar.domain.contains(oldValOfFirstVar)
          && firstVar.domain.contains(oldValOfSecondVar)) {

          this.firstVar = firstVar
          this.secondVar = secondVar
          this.firstVarIndice = i
          this.secondVarIndice = j
          if (evaluateCurrentMoveObjTrueIfStopRequired(obj.swapVal(firstVar, secondVar))) {
            startIndice = i + 1
            return
          }
        }
      }
    }
  }

  var firstVar:CBLSIntVar = null
  var secondVar:CBLSIntVar = null
  var firstVarIndice:Int = 0
  var secondVarIndice:Int = 0

  override def instantiateCurrentMove(newObj: Int) = SwapMove(firstVar, secondVar, firstVarIndice,secondVarIndice,newObj, name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
 * Will randomize the array, typically to get out of a local minimal
 * This will not consider the objective function, even if it includes some strong constraints
 *
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param valuesToConsider: the set of values to consider for the given variable
 * @param name the name of the neighborhood
 */
case class RandomizeNeighborhood(vars:Array[CBLSIntVar],
                                 degree:Int = 1,
                                 name:String = "RandomizeNeighborhood",
                                 searchZone:() => SortedSet[Int] = null,
                                 valuesToConsider:(CBLSIntVar,Int) => Iterable[Int] = (variable,_) => variable.domain)
  extends Neighborhood(name) with AlgebraTrait with SearchEngineTrait{

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean = null): SearchResult = {
    if(printPerformedSearches) println("applying " + name)

    var toReturn:List[Move] = List.empty

    if(searchZone != null && searchZone().size <= degree){
      //We move everything
      for(i <- searchZone()){

        toReturn = AssignMove(vars(i),selectFrom(vars(i).domain),i,Int.MaxValue) :: toReturn
      }
    }else{
      var touchedVars:Set[Int] = SortedSet.empty
      for(r <- 1 to degree){
        val i = selectFrom(vars.indices,(j:Int) => (searchZone == null || searchZone().contains(j)) && !touchedVars.contains(j))
        touchedVars = touchedVars + i
        val oldVal = vars(i).value
        toReturn = AssignMove(vars(i),selectFrom(valuesToConsider(vars(i),i),(_:Int) != oldVal),i,Int.MaxValue) :: toReturn
      }
    }
    if(printPerformedSearches) println(name + ": move found")
    CompositeMove(toReturn, Int.MaxValue, name)
  }
}

/**
 * will randomize the array, by performing swaps only.
 * This will not consider the objective function, even if it includes some strong constraints
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param degree the number of variables to change randomly
 * @param searchZone a subset of the indices of vars to consider.
 *                   If none is provided, all the array will be considered each time
 * @param name the name of the neighborhood
 */
case class RandomSwapNeighborhood(vars:Array[CBLSIntVar],
                                  degree:Int = 1,
                                  name:String = "RandomSwapNeighborhood",
                                  searchZone:() => SortedSet[Int] = null)  //TODO: search zone does not work!
  extends Neighborhood(name) with AlgebraTrait with SearchEngineTrait{

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean = null): SearchResult = {
    if(printPerformedSearches) println("applying " + name)

    var toReturn:List[Move] = List.empty

    var touchedVars:Set[Int] = SortedSet.empty
    val varsToMove = if (searchZone == null) vars.length else searchZone().size
    for(r <- 1 to degree if varsToMove - touchedVars.size >= 2){
      val i = selectFrom(vars.indices,(i:Int) => (searchZone == null || searchZone().contains(i)) && !touchedVars.contains(i))
      touchedVars = touchedVars + i
      val j = selectFrom(vars.indices,(j:Int) => (searchZone == null || searchZone().contains(j)) && !touchedVars.contains(j))
      touchedVars = touchedVars + j
      toReturn = SwapMove(vars(i), vars(j), i,j,Int.MaxValue) :: toReturn
    }

    if(printPerformedSearches) println(name + ": move found")
    CompositeMove(toReturn, Int.MaxValue, name)
  }
}

/**
 * will randomize the array, by performing shuffle on a subset of the variables
 * This will not consider the objective function, even if it includes some strong constraints
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param indicesToConsider the positions to consider in the shuffle, all positions if not specified
 * @param numberOfShuffledPositions the number of positions to shuffle, taken in indicesToConsider.
 * @param name the name of the neighborhood
 * @param checkNoMoveFound checks that the variables to shuffle have different values, return NoMoveFound if this is not the case
 */
case class ShuffleNeighborhood(vars:Array[CBLSIntVar],
                               indicesToConsider:()=>Iterable[Int] = null,
                               numberOfShuffledPositions:() => Int = () => Int.MaxValue,
                               name:String = "ShuffleNeighborhood",
                               checkNoMoveFound:Boolean = true)
  extends Neighborhood(name) with AlgebraTrait with SearchEngineTrait{

  override def getMove(obj: Objective, acceptanceCriteria: (Int, Int) => Boolean = null): SearchResult = {
    if(printPerformedSearches) println("applying " + name)

    val (realIndicesToConsider:List[Int],numberOfIndicesToConsider:Int) =
      (if(indicesToConsider == null) (vars.indices.toList,vars.length)
      else { val tmp = indicesToConsider(); (tmp.toList,tmp.size)})

    if(checkNoMoveFound) {
      val (minValue, maxValue) = InvariantHelper.getMinMaxBoundsInt(realIndicesToConsider.map(vars(_).value))
      if (minValue == maxValue) return NoMoveFound
    }

    val numberOfShuffledPositionsThisTime = numberOfShuffledPositions()
    val subsetOfIndicesToConsider:List[Int] = if(numberOfShuffledPositionsThisTime >= numberOfIndicesToConsider){
      realIndicesToConsider
    }else{
      //shuffle only a subset; select it randomly
      Random.shuffle(realIndicesToConsider).takeRight(numberOfShuffledPositionsThisTime)
    }

    //shuffle everything
    val values = subsetOfIndicesToConsider.map(vars(_).value)
    val newValues = Random.shuffle(values)

    val moves:List[AssignMove] = subsetOfIndicesToConsider.zip(newValues).
      map({case ((indice,newValue)) => AssignMove(vars(indice),newValue,indice,Int.MaxValue)})

    if(printPerformedSearches) println(name + ": move found")
    CompositeMove(moves, Int.MaxValue, name)
  }
}

/**
 * This neighborhood will consider roll moves that roll the value of contiguous CBLSIntVar in the given array
 *
 * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
 * @param searchZone a subset of the indices of vars to consider for the roll
 *                   If none is provided, all the array will be considered each time
 * @param bridgeOverFrozenVariables if false, contiguous variables are the ones that are adjacent in the array,
 *                                  so that if a variable is not in the search zone,
 *                                  no roll can involve vars on its left and on its right
 *                                  if true, variable in the search zone will simply be ignored
 * @param maxShiftSize the max size of the roll, given the first indice considered in the roll
 * @param name the name of the neighborhood
 * @param hotRestart  if true, the exploration order in case you ar not going for the best
 *                    is a hotRestart for the first swapped variable
 *                    even if you specify a searchZone that is: the exploration starts again
 *                    at the position where it stopped, and consider the indices in increasing order
 *                    if false, consider the exploration range in natural order from the first position.
 **/
@deprecated("actually, experimental, so use at your own risk","3.0")
//TODO: also implement minimal roll size (we prefer to use swap instead of roll)
case class RollNeighborhood(vars:Array[CBLSIntVar],
                            name:String = "RollNeighborhood",
                            searchZone:()=>Set[Int] = null,
                            bridgeOverFrozenVariables:Boolean = false,
                            maxShiftSize:Int=>Int = _ => Int.MaxValue, //the max size of the roll, given the ID of the first variable
                           //minRollSize:Int, //TODO
                            best:Boolean = false,
                            hotRestart:Boolean = true)
  extends EasyNeighborhood[RollMove](best,name) with AlgebraTrait{
  //the indice to start with for the exploration
  var startIndice:Int = 0
  override def exploreNeighborhood(){

    val searchZoneObject = if(searchZone == null) null else searchZone()
    val currentSearchZone = if(searchZone == null) vars.indices else searchZoneObject

    @inline
    def searchZoneContains(i:Int):Boolean = {
      if(searchZone == null) vars.indices.contains(i) else searchZoneObject.contains(i)
    }

    val firstIndices = if (hotRestart && !best)
      HotRestart(currentSearchZone, startIndice)
    else currentSearchZone

    for (firstIndice: Int <- firstIndices) {

      val currentMaxShiftSize = maxShiftSize(firstIndice)

      var currentEnd = firstIndice
      var currentRollSize = 1
      currentRollCluster = List(vars(currentEnd))
      initValue = List(vars(currentEnd).value)

      def advance(lastIndice:Int):(Int,Boolean) = {
        if(currentRollSize >= currentMaxShiftSize) return (0,false)
        val potentialAdd = currentEnd+1
        if(potentialAdd >= vars.length) return (0,false)
        if(searchZoneContains(potentialAdd)){
          (potentialAdd,true)
        }else if(bridgeOverFrozenVariables){
          advance(potentialAdd)
        }else{
          (0,false)
        }
      }

      var (newEnd,isNewEnd) = advance(currentEnd)
      while(isNewEnd){
        //updating roll
        currentEnd = newEnd
        currentRollSize +=1
        currentRollCluster = vars(currentEnd) :: currentRollCluster
        initValue = vars(currentEnd).value :: initValue

        //performing rolls

        rollOffset = 1
        while(rollOffset < currentRollSize){
          //check this roll
          doRollOneLeft()
          if (evaluateCurrentMoveObjTrueIfStopRequired(obj.value)) {
            startIndice = advance(firstIndice)._1
            assignAll(currentRollCluster,initValue)
            return
          }
          rollOffset += 1
        }
        assignAll(currentRollCluster,initValue)

        val tmp = advance(currentEnd)
        newEnd = tmp._1
        isNewEnd = tmp._2
      }
    }
  }

  var rollOffset:Int = 0
  var currentRollCluster:List[CBLSIntVar] = List.empty
  var initValue:List[Int] = List.empty

  def doRollOneLeft(): Unit ={
    val i = currentRollCluster.head.value
    var currentPos = currentRollCluster
    while(true){
      currentPos match{
        case h1::h2::t =>
          h1 := h2.value
          currentPos = h2 :: t
        case h1 :: Nil =>
          h1 := i
          return
        case Nil => throw new Error("empty roll?")
      }
    }
  }

  def assignAll(vars:List[CBLSIntVar],vals:List[Int]){
    (vars, vals) match {
      case (hVar :: t1, hVal :: t2) =>
        hVar := hVal
        assignAll(t1,t2)
      case (Nil,Nil) => return
      case _ => throw new Error("bug in assignAll")
    }
  }

  override def instantiateCurrentMove(newObj: Int) =
    RollMove(currentRollCluster,rollOffset,newObj,name)

  //this resets the internal state of the Neighborhood
  override def reset(): Unit = {
    startIndice = 0
  }
}

/**
  * will shift a block of value to the right(doing it also to the left is redundant)
  *
  * @param vars an array of [[oscar.cbls.invariants.core.computation.CBLSIntVar]] defining the search space
  * @param searchZone a subset of the indices of vars to consider in order to determine the block's extremities
  *                   if none provided, all the array will be considered each time
  * @param startShiftIndice the indice of the value that will start the shift block
  *                         if no startShiftIndice value is given, the neighborhood will consider every possible value in the given array
  * @param endShiftIndice the indice of the value that will end the shift bloc (similar to the previous setting)
  * @param maxShiftSize the max size of the shift, given the first indice considered in the shift
  * @param maxOffsetSize the max size of the offset
  * @param best if true, the neighborhood will try to find the best solution possible
  *             (not very usefull because browsing all the possibilities can be very long)
  * @param name the name of the neighborhood
  * @param hotRestart  if true, the exploration order in case you ar not going for the best
  *                    is a hotRestart for the first swapped variable
  *                    even if you specify a searchZone that is: the exploration starts again
  *                    at the position where it stopped, and consider the indices in increasing order
  *                    if false, consider the exploration range in natural order from the first position.
  * @author fabian.germeau@student.vinci.be
  **/
case class ShiftNeighborhood(vars:Array[CBLSIntVar],
                             name:String = "ShiftNeighborhood",
                             searchZone:()=>Iterable[Int] = null,
                             startShiftIndice:Int = -1,
                             endShiftIndice:Int = -1,
                             maxShiftSize:Int = Int.MaxValue,
                             maxOffsetSize:Int = Int.MaxValue,
                             best:Boolean = false,
                             hotRestart: Boolean = true)
  extends EasyNeighborhood[ShiftMove](best,name) with AlgebraTrait{
  /**
    * This is the method you must implement and that performs the search of your neighborhood.
    * every time you explore a neighbor, you must perform the calls to notifyMoveExplored or moveRequested(newObj) && submitFoundMove(myMove)){
    * as explained in the documentation of this class
    */
  var startIndice:Int = 0
  var currentShiftOffset:Int = 0
  var currentShiftSize:Int = 1
  var currentDirection:Int = 1
  var currentStart:Int = 0
  override def exploreNeighborhood(){
    val searchZoneObject = if(searchZone == null)null else searchZone()
    val currentSearchZone = if(searchZone == null)vars.indices else searchZoneObject


    val firstIndices =
      if(hotRestart && !best)HotRestart(currentSearchZone, startIndice)
      else if(startShiftIndice != -1)List(startShiftIndice)
      else currentSearchZone

    //We first determine the border of the shift block and then we determine the movement to perform
    for(firstIndice: Int <- firstIndices){
      currentStart = firstIndice
      for(secondIndice: Int <- currentSearchZone){
        val currentEnd = secondIndice
        currentShiftSize = currentEnd-currentStart
        currentShiftOffset = 1
        while (currentShiftOffset < vars.length - currentEnd) {
          val newObj = doShiftNeighborhood(vars.toList,currentStart,currentShiftSize,currentShiftOffset)
          if(evaluateCurrentMoveObjTrueIfStopRequired(newObj)){
            startIndice = (currentStart + 1)%vars.length
            return
          }
          currentShiftOffset += 1
        }
      }
    }



    /**returns the value of the objective variable if the block of value specified
      * by startIndice and length is moved by offset positions to the right
      * this process is efficiently performed as the objective Variable is registered for partial propagation
      * @see registerForPartialPropagation() in [[oscar.cbls.invariants.core.computation.Store]]
      */
    def doShiftNeighborhood(l:List[CBLSIntVar],startIndice:Int,length:Int,offset:Int): Int ={
      val variables = l.toArray
      val initialValues: Array[Int] = l.toArray.map(_.value)
      for (i <- startIndice to startIndice + offset + length - 1) {
        if (i < startIndice + offset) {
          variables(i) := initialValues(i + length)
        }
        else {
          variables(i) := initialValues(i - offset)
        }
      }
      val newVal = obj.value
      for(i <- variables)i:=initialValues(variables.indexOf(i))
      newVal
    }
  }

  override def instantiateCurrentMove(newObj: Int) = ShiftMove(currentStart,currentShiftSize,currentShiftOffset,vars,newObj,name)

  override def reset(): Unit = {
    startIndice = 0
  }
}