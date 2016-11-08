package oscar.cbls.algo.fun
/**
 * *****************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 * ****************************************************************************
 */

import oscar.cbls.algo.rb.{RedBlackTreeMap, RedBlackTreeMapExplorer}

import scala.language.implicitConversions

object PiecewiseLinearFun{
  def identity = new PiecewiseLinearFun()
  implicit def toIterable(f:PiecewiseLinearFun):Iterable[Pivot] = f.transformation.values
  def createFromPivots(pivots:Iterable[Pivot]):PiecewiseLinearFun = {
    var acc = RedBlackTreeMap.empty[Pivot]
    val pivotIt = pivots.toIterator
    while(pivotIt.hasNext){
      val currentPivot = pivotIt.next()
      acc =  acc.insert(currentPivot.fromValue,currentPivot)
    }
    new PiecewiseLinearFun(acc)
  }
}

class CachedPiecewiseLinearFun(f:PiecewiseLinearFun) extends PiecewiseLinearFun(f.transformation){

  var cachedPivotF:LinearTransform = null
  var cachedPivotStart:Int = 0
  var cachedPivotEnd:Int = -1

  override def apply(value:Int):Int = {
    val toReturn = if(cachedPivotF != null && (cachedPivotStart <= value) && (value <= cachedPivotEnd)){
      cachedPivotF(value)
    }else if(cachedPivotF == null && cachedPivotEnd>=value){
      value
    }else {
      transformation.biggestLowerOrEqual(value) match {
        case None =>
          cachedPivotEnd = transformation.smallestBiggerOrEqual(value) match {
            case None => Int.MaxValue
            case Some((kAbove, _)) => kAbove-1
          }
          value
        case Some((k,pivot)) =>
          cachedPivotStart = k
          cachedPivotEnd = transformation.smallestBiggerOrEqual(value) match {
            case None => Int.MaxValue
            case Some((kAbove, _)) => kAbove-1
          }
          cachedPivotF = pivot.f
          cachedPivotF(value)
      }
    }
    assert(toReturn == super.apply(value))
    toReturn
  }
}


class PiecewiseLinearFun(private[fun] val transformation: RedBlackTreeMap[Pivot] = RedBlackTreeMap.empty) {

  def firstPivot:Option[(Int,Pivot)] = transformation.smallestBiggerOrEqual(Int.MinValue)

  def positionOfValue(value:Int):Option[RedBlackTreeMapExplorer[(Pivot)]] = transformation.positionOf(value)

  def isIdentity = transformation.isEmpty

  def nbPivot = transformation.size

  def pivots:List[Pivot] = transformation.values

  override def toString: String = {
    "PiecewiseLinearFun(nbSegments:" + transformation.size + ", " + (if(transformation.isEmpty) "identity" else ("segments:" + transformation.values.mkString(",")))+")"
  }

  def apply(value:Int):Int = {
    transformation.biggestLowerOrEqual(value) match {
      case None => value
      case Some((_,pivot)) => pivot.f(value)
    }
  }

  def updateForCompositionBefore(updates:(Int,Int,LinearTransform)*):PiecewiseLinearFun = {
    var updatedTransform=transformation
    for((fromIncluded,toIncluded,update) <- updates) {
      updatedTransform = deleteUnnecessaryPivotStartingJustAfter(toIncluded,myUpdateForCompositionBefore(fromIncluded, toIncluded, update, removePivotsBetween(fromIncluded, toIncluded, updatedTransform)))
    }
    new PiecewiseLinearFun(updatedTransform)
  }


  def updateForCompositionBefore(fromIncluded:Int, toIncluded: Int, additionalFAppliedBefore: LinearTransform):PiecewiseLinearFun = {
    //step1: remove all pivots between fromIncluded and toIncluded
    //step2: recall the linear transform that was at the end, because we will need to re-put this one with an additional pivot, except if we get the same function with our new pivots
    //Step3: generate, and store all the new pivots
    //step4: add a finishing pivot if necessary (cfr. step2)

    //remove all the pivots between fromIncluded and ToIncluded
    val cleanedTransformation = removePivotsBetween(fromIncluded, toIncluded, transformation)
    val updatedTransform = myUpdateForCompositionBefore(fromIncluded, toIncluded, additionalFAppliedBefore,cleanedTransformation)
    val updatedTransformDeletedExtraPivot = deleteUnnecessaryPivotStartingJustAfter(toIncluded,updatedTransform)
    new PiecewiseLinearFun(updatedTransformDeletedExtraPivot)
  }

  private def deleteUnnecessaryPivotStartingJustAfter(toIncluded:Int,updatedTransform:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {
    updatedTransform.get(toIncluded+1) match{
      case None => updatedTransform
      case Some(pivotStartingJustAfterToIncluded) =>
        updatedTransform.biggestLowerOrEqual(toIncluded) match{
          case None => if (pivotStartingJustAfterToIncluded.f.isIdentity) updatedTransform.remove(toIncluded+1) else updatedTransform
          case Some((_,pivotApplyingAtToIncluded)) =>
            if (pivotStartingJustAfterToIncluded.f equals pivotApplyingAtToIncluded.f) updatedTransform.remove(toIncluded+1) else updatedTransform
        }
    }
  }


  private def myUpdateForCompositionBefore(fromIncluded:Int, toIncluded: Int, additionalFAppliedBefore: LinearTransform,cleanedTransformation:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {
    val isAdditionaFNegativeSlope = additionalFAppliedBefore.minus

    var currentFromIncluded = fromIncluded
    var currentTransformation = cleanedTransformation
    var currentIncludedFromAfterAdditionalF = additionalFAppliedBefore(currentFromIncluded)
    var positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF =
      transformation.biggestLowerOrEqual(currentIncludedFromAfterAdditionalF) match {
        case None => None //no pivot applying at this point, consider identity function... until when?
        case Some((key, _)) => transformation.positionOf(key)
      }

    while(currentFromIncluded <= toIncluded) {
      positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF match {
        case None => //no pivot at current point, so identity is assumed
          val (nextCurrentIncludedFrom, nextPivotPosition) = if (isAdditionaFNegativeSlope) {
            (toIncluded+1, None)
          } else {
            val nextPivotPosition:Option[RedBlackTreeMapExplorer[Pivot]] = transformation.smallestPosition
            (nextPivotPosition match{case None => toIncluded+1 case Some(position) => additionalFAppliedBefore.unApply(position.value.fromValue)},
              nextPivotPosition)
          }

          //delivers a new linear transform that is equal to this(that(value))
          val newF = additionalFAppliedBefore

          //Adds the pivot (only if different transform of course)
          if (currentTransformation.biggestLowerOrEqual(currentFromIncluded) match {
            case None => !newF.isIdentity
            case Some((key, existingPivot)) => !existingPivot.f.equals(newF)
          }){
            currentTransformation = currentTransformation.insert(currentFromIncluded, new Pivot(currentFromIncluded, newF))
          }

          currentFromIncluded = nextCurrentIncludedFrom
          currentIncludedFromAfterAdditionalF = additionalFAppliedBefore(currentFromIncluded)
          positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF = nextPivotPosition
        case Some(position) =>
          //compute end of pivot (start is currentFromIncluded)
          val (nextCurrentIncludedFrom, nextPivotPosition) = if (isAdditionaFNegativeSlope) {
            (additionalFAppliedBefore.unApply(position.value.fromValue - 1), position.prev)
          } else {
            val nextPivotPosition = position.next
            (nextPivotPosition match{case None => toIncluded+1 case Some(position) => additionalFAppliedBefore.unApply(position.value.fromValue)},
              nextPivotPosition)
          }

          //delivers a new linear transform that is equal to this(that(value))
          val newF = position.value.f(additionalFAppliedBefore)

          //Adds the pivot (only if different transform of course)
          if (currentTransformation.biggestLowerOrEqual(currentFromIncluded) match {
            case None => !newF.isIdentity
            case Some((key, existingPivot)) => !existingPivot.f.equals(newF)
          }){
            currentTransformation = currentTransformation.insert(currentFromIncluded, new Pivot(currentFromIncluded, newF))
          }

          currentFromIncluded = nextCurrentIncludedFrom
          currentIncludedFromAfterAdditionalF = additionalFAppliedBefore(currentFromIncluded)
          positionOfPivotApplyingOnCurrentIncludedFromAfterAdditionalF = nextPivotPosition

        //compute end of this section
      }
    }
    currentTransformation
  }


  /**
   * removes all pivots between fromIncluded and toIncluded
   * also adds a pivot at toIncluded+1 if necessary to ensure that values starting at toIncluded+1 onwards are not impacted
   */
  private def removePivotsBetween(fromIncluded:Int,toIncluded:Int, transformToClean:RedBlackTreeMap[Pivot]):RedBlackTreeMap[Pivot] = {

    val transformWithAddedFinishingPivot = transformToClean.biggestLowerOrEqual(toIncluded + 1) match {
      case Some((key, pivot)) => transformToClean.insert(toIncluded + 1, new Pivot(toIncluded + 1, pivot.f))
      case _ => transformToClean.insert(toIncluded + 1, new Pivot(toIncluded + 1, LinearTransform.identity))
    }

    var currentCleanedTransform = transformWithAddedFinishingPivot
    var pivotToRemove = currentCleanedTransform.biggestLowerOrEqual(toIncluded)
    while (true) {
      pivotToRemove match {
        case Some((key, pivot)) if key >= fromIncluded =>
          currentCleanedTransform = currentCleanedTransform.remove(key)
          pivotToRemove = currentCleanedTransform.biggestLowerOrEqual(toIncluded)
        case _ => return currentCleanedTransform
      }
    }
    null
  }

  def pivotWithPositionApplyingTo(value:Int):Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.biggestLowerOrEqual(value) match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def pivotWithPositionAfter(value:Int):Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.smallestBiggerOrEqual(value) match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def pivotApplyingTo(value:Int):Option[Pivot] = {
    transformation.biggestLowerOrEqual(value)  match {
      case None => None
      case Some((_, p)) => Some(p)
    }
  }

  def pivotAfter(value:Int):Option[Pivot] = {
    transformation.smallestBiggerOrEqual(value)  match {
      case None => None
      case Some((_, p)) => Some(p)
    }
  }

  def firstPivotAndPosition:Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.smallest match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def lastPivotAndPosition:Option[RedBlackTreeMapExplorer[Pivot]] = {
    transformation.biggest match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }




  def composeAfter(fromIncluded: Int, toIncluded: Int, additionalFAppliedAfter: LinearTransform): PiecewiseLinearFun = {
    if(additionalFAppliedAfter.isIdentity) this
    else new PiecewiseLinearFun(updatePivotsForCompositionAfter(fromIncluded, toIncluded, additionalFAppliedAfter))
  }

  private def updatePivotsForCompositionAfter(fromIncluded: Int, toIncluded: Int, additionalFAppliedAfter: LinearTransform): RedBlackTree[Pivot] = {
    //println("updatePivotsForCompositionAfter(from:" + fromIncluded + ", to:" + toIncluded + ", fct:" + additionalFAppliedAfter + ")")

    transformation.getBiggestLowerOrEqual(fromIncluded) match {
      case Some((_,pivot)) if (pivot.fromValue == fromIncluded) =>
        updateFromPivotForCompositionAfter(pivot, toIncluded, additionalFAppliedAfter, transformation)
      case Some((_,pivot)) =>
        //there is a pivot below the point
        //need to add an intermediary pivot, with same transform as previous one
        val newPivot = new Pivot(fromIncluded, pivot.f)
        updateFromPivotForCompositionAfter(newPivot, toIncluded, additionalFAppliedAfter, transformation.insert(fromIncluded, newPivot))
      case None =>
        transformation.getSmallestBiggerOrEqual(fromIncluded) match{
          case None =>
            //need to add a first pivot from this point
            val newPivot = new Pivot(fromIncluded, LinearTransform.identity)
            updateFromPivotForCompositionAfter(newPivot, toIncluded, additionalFAppliedAfter, transformation.insert(fromIncluded, newPivot))
          case Some((_,next)) =>
            val newPivot = new Pivot(fromIncluded, LinearTransform.identity)
            updateFromPivotForCompositionAfter(newPivot, toIncluded, additionalFAppliedAfter,transformation.insert(fromIncluded, newPivot))
        }
    }
  }

  private def updateFromPivotForCompositionAfter(pivot: Pivot, toIncluded: Int, additionalFAppliedAfter: LinearTransform, transformation: RedBlackTree[Pivot]):RedBlackTree[Pivot] = {
    if (pivot.fromValue == toIncluded+1) return transformation //finished the correction

    val previousCorrection = pivot.f
    val newCorrection = additionalFAppliedAfter(previousCorrection)

    val newPivot = new Pivot(pivot.fromValue,newCorrection)
    val transformWithNewPivot = transformation.insert(pivot.fromValue,newPivot)

    val prevPivot = transformWithNewPivot.getBiggestLowerOrEqual(pivot.fromValue-1)

    val removeCurrentPivot = prevPivot match{
      case None => newCorrection.isIdentity
      case Some((fromValue,pivot)) =>
        pivot.f.equals(newCorrection)
    }

    val(newPrev,newTransform) = if(removeCurrentPivot){
      (prevPivot,transformWithNewPivot.remove(pivot.fromValue))
    }else{
      (Some(pivot.fromValue,newPivot),transformWithNewPivot)
    }

    newTransform.getSmallestBiggerOrEqual(pivot.fromValue+1) match{
      case None =>
        if (newPrev == null) newTransform
        //We have an open correction, and need to close it with the previous value previousCorrection
        else newTransform.insert(toIncluded+1, new Pivot(toIncluded+1, previousCorrection))
      case Some((nextFromValue,nextPivot)) =>
        if (nextFromValue > toIncluded + 1) {
          //need to add a new intermediary pivot
          newPrev match{
            case None =>
              newTransform
            case Some((newPrevFromValue,newPrevPivot)) =>
              if (newPrevPivot.f.equals(previousCorrection)) newTransform
              else newTransform.insert(toIncluded + 1, new Pivot(toIncluded + 1, previousCorrection))
          }
        } else if (nextFromValue < toIncluded+1){
          //there is a next such that next.value is <= correctedTo
          //so recurse to it
          updateFromPivotForCompositionAfter(nextPivot, toIncluded, additionalFAppliedAfter,newTransform)
        }else {
          //check that next pivot should not be removed, actually
          newPrev match {
            case None if nextPivot.f.isIdentity => newTransform.remove(nextFromValue)
            case Some((newPrevFromValue, newPrevPivot)) if nextPivot.f.equals(newPrevPivot.f) => newTransform.remove(nextFromValue)
            case _ => newTransform
          }
        }
    }
  }


}

class Pivot(val fromValue:Int, val f: LinearTransform){
  override def toString = "Pivot(from:" + fromValue + " " + f + ")"
}
