package oscar.cbls.lib.invariant.minmax

import oscar.cbls.algo.heap.BinomialHeapWithMoveInt
import oscar.cbls._
import oscar.cbls.core._

import scala.collection.immutable.SortedSet

/**
 *
 * @param constArray
 * @param condSet
 * @param default
 * @param maxDiameter is the maximal number of values in condSet that are monitored in the set, must be >=1.
 *                    the actual diameter is kept between 1 and tis value, lazily
 */
class MinConstArrayValueWise(constArray: Array[Int], condSet: SetValue, default: Int, maxDiameter:Int = 2)
  extends IntInvariant with SetNotificationTarget{

  private val n = constArray.length
  private val range = 0 until n

  //TODO: this is very slow!
  private val idSortedByIncreasingValue:Array[Int] = range.toArray.sortBy(a => constArray(a))

  private var nbListenedVals = 0
  private val idToTheirPositionNumber:Array[Int] = Array.fill(n)(-1)
  for(i <- range){
    idToTheirPositionNumber(idSortedByIncreasingValue(i)) = i
  }

  //TODO: should be total registration in case there are less than, say, 10 values in the set!

  registerStaticDependency(condSet)
  private val key:ValueWiseKey = registerDynamicValueWiseDependency(condSet)
  finishInitialization()

  //only the smallest position in cond
  //we listen to these ones (and to the no-included smaller ones as well)
  private val heapOfConsideredPositions = new BinomialHeapWithMoveInt(i => constArray(i),n, n)

  //GetKey:T => Int,val maxsize:Int, position:scala.collection.mutable.Map[T,Int])(implicit val A:Ordering[T],implicit val X:Manifest[T]){
  addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(condSet.value)

  this := (if(heapOfConsideredPositions.isEmpty) default else constArray(heapOfConsideredPositions.getFirst))

  override def notifySetChanges(v : ChangingSetValue, d : Int,
                                addedValues : Iterable[Int],
                                removedValues : Iterable[Int],
                                oldValue : SortedSet[Int],
                                newValue : SortedSet[Int]){
    //we start wit the added because we want to avoid exploring the values for nothing
    require(d == Int.MinValue)

    val removedValuesIt = removedValues.iterator
    while(removedValuesIt.hasNext){
      val deleted = removedValuesIt.next()
      if(idToTheirPositionNumber(deleted) < nbListenedVals) {
        heapOfConsideredPositions.delete(deleted)
      }
    }

    val addedValuesIt = addedValues.iterator
    while(addedValuesIt.hasNext){
      val added = addedValuesIt.next()
      if(idToTheirPositionNumber(added) < nbListenedVals) {
        heapOfConsideredPositions.insert(added)
      }
    }

    val diameter = heapOfConsideredPositions.size
    if(diameter == 0){
      //increase diameter to 1
      addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(newValue)
    }else if (diameter > maxDiameter){
      //reduce diameter
      removeValuesFromListenedUntilDiameterSmallerOrEqualToMaxDiameter()
      //trimNotListenedTail()
    }

    //update this value
    this := (if(heapOfConsideredPositions.isEmpty) default else constArray(heapOfConsideredPositions.getFirst))
  }

  /**
   * adds one more value into the scope,
   * does not add into the heapOfConsideredPositions
   * @return the iD added into the scope
   */
  private def addOneMoveValueIntoScope():Int = {
    val IdToAddIntoTheScope = idSortedByIncreasingValue(nbListenedVals)
    nbListenedVals += 1
    key.addToKey(IdToAddIntoTheScope)
    IdToAddIntoTheScope
  }

  private def removeValuesFromListenedUntilDiameterSmallerOrEqualToMaxDiameter(){
    while(heapOfConsideredPositions.size > maxDiameter) {
      require(nbListenedVals >0)
      val removedValue = idSortedByIncreasingValue(nbListenedVals-1)
      nbListenedVals -= 1
      key.removeFromKey(removedValue)
      heapOfConsideredPositions.deleteIfPresent(removedValue)
    }
  }

  private def trimNotListenedTail(){
    while(true) {
      val removedValue = idSortedByIncreasingValue(nbListenedVals-1)
      if(heapOfConsideredPositions.contains(removedValue)) return
      nbListenedVals -= 1
      key.removeFromKey(removedValue)
      heapOfConsideredPositions.deleteIfPresent(removedValue)
    }
  }

  private def addValuesIntoListenedUntilDiameterIsStrictlyBiggerThanZeroOrFullScope(condValue:SortedSet[Int]){
    while(heapOfConsideredPositions.size == 0 && nbListenedVals < n) {
      val addedValue = addOneMoveValueIntoScope()
      if(condValue contains addedValue) {
        heapOfConsideredPositions.insert(addedValue)
      }
    }
  }

  override def checkInternals(c : Checker) {
    if (condSet.value.isEmpty) c.check(value == default)
    else c.check(value == constArray(condSet.value.minBy(constArray(_))))
    c.check(nbListenedVals >= heapOfConsideredPositions.size)
  }
}

