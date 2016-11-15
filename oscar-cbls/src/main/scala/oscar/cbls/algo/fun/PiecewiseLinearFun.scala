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

  def areEqual(a:PiecewiseLinearFun,b:PiecewiseLinearFun):Boolean = {
    if(a.nbPivot != b.nbPivot) return false
    val zippedSegments = a.zip(b)
    for((pivotA,pivotB) <- zippedSegments){
      if (pivotA.fromValue != pivotB.fromValue) return false
      if(! (pivotA.f equals pivotB.f)) return false
    }
    return true
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

  def equals(that:PiecewiseLinearFun):Boolean = {
    PiecewiseLinearFun.areEqual(this,that)
  }

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

  def setPivot(fromIncluded:Int,toIncluded:Int,f:LinearTransform):PiecewiseLinearFun = {
    //First, delete all pivots between fromValue and toValue
    val cleanedFun = removePivotsBetween(fromIncluded,toIncluded, transformation)
    //then check the previous pivot, to see if it aligns with the desired f
    val applyingFunction = cleanedFun.biggestLowerOrEqual(fromIncluded) match{
      case None => LinearTransform.identity
      case Some((pivotStart,pivot)) => pivot.f
    }
    if(applyingFunction equals f){
      new PiecewiseLinearFun(deleteUnnecessaryPivotStartingJustAfter(toIncluded,cleanedFun))
    }else{
      //just add the pivot
      //and check if it is aligned with the segment after the zone; in this case this pivot can be deleted
      new PiecewiseLinearFun(deleteUnnecessaryPivotStartingJustAfter(toIncluded,cleanedFun.insert(fromIncluded,new Pivot(fromIncluded,f))))
    }
  }
}

class Pivot(val fromValue:Int, val f: LinearTransform){
  override def toString = "Pivot(from:" + fromValue + " " + f + ")"
}
