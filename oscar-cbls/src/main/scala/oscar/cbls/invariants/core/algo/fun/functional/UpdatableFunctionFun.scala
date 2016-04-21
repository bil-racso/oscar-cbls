package oscar.cbls.invariants.core.algo.fun.functional

import oscar.cbls.invariants.core.algo.fun.mutable.{LinearTransform}
import oscar.cbls.invariants.core.algo.rb.{RBPosition, RedBlackTree}
import scala.language.implicitConversions

object PiecewiseLinearFun{
  def identity = new PiecewiseLinearFun()
  implicit def toIterable(f:PiecewiseLinearFun):Iterable[Pivot] = f.transformation.values
  def createFromPivots(pivots:Iterable[Pivot]):PiecewiseLinearFun = {
    var acc = RedBlackTree.empty[Pivot]
    val pivotIt = pivots.toIterator
    while(pivotIt.hasNext){
      val currentPivot = pivotIt.next()
      acc =  acc.insert(currentPivot.fromValue,currentPivot)
    }
    new PiecewiseLinearFun(acc)
  }
}

class PiecewiseLinearFun(private[fun] val transformation: RedBlackTree[Pivot] = RedBlackTree.empty) {

  def firstPivot:Option[(Int,Pivot)] = transformation.getSmallestBiggerOrEqual(Int.MinValue)

  def positionOfValue(value:Int):Option[RBPosition[(Pivot)]] = transformation.positionOf(value)

  def isIdentity = transformation.isEmpty

  def nbPivot = transformation.size

  def pivots:List[Pivot] = transformation.values

  override def toString: String = {
    "PiecewiseLinearFun(nbSegments:" + transformation.size + ", " + (if(transformation.isEmpty) "identity" else ("segments:" + transformation.values.mkString(",")))+")"
  }

  def apply(value:Int):Int = {
    transformation.getBiggestLowerOrEqual(value) match {
      case None => value
      case Some((_,pivot)) => pivot.f(value)
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


  def updateForCompositionBefore(fromIncluded:Int, toIncluded: Int, additionalFAppliedBefore: LinearTransform):RedBlackTree[Pivot] = {
    //step1: remove all pivots between fromIncluded and toIncluded
    //step2: recall the linear transform that was at the end, because we will need to re-put this one with an additional pivot, except if we get the same function with our new pivots
    //Step3: generate, and store all the new pivots
    //step4: add a finishing pivot if necessary (cfr. step2)

    //remove all the pivots between fromIncluded and ToIncluded
    val previousFinishingPivot:Option[Pivot] = pivotApplyingTo(toIncluded)
    val cleanedTransformation = removePivotsBetween(fromIncluded,previousFinishingPivot)

    //generate, and store all the new pivots
    if(additionalFAppliedBefore.minus){
      //negative slope for the additional transform
    }else{
      //positive slope for the additional transform
      var currentPositionAfterAdditionalF = additionalFAppliedBefore(fromIncluded)
      var currentPivotWithPosition:Option[RBPosition[Pivot]] = pivotWithPositionApplyingTo(currentPositionAfterAdditionalF)
      while(true){
        currentPivotWithPosition match{
          case None =>

          case Some(pivot) =>

        }
      }
    }

  }



  def removePivotsBetween(fromIncluded:Int,firstPivotToRemove:Option[Pivot]):RedBlackTree[Pivot] = {
    var currentPivotToRemove = firstPivotToRemove
    var currentCleanedTransform = transformation
    while (currentPivotToRemove match {
      case None => false
      case Some(p) if p.fromValue >= fromIncluded
    }) {
      currentCleanedTransform = currentCleanedTransform.remove(currentPivotToRemove.head.fromValue)
    }
    (currentCleanedTransform)
  }

  private def updateFromPivotForCompositionBefore(prevUpdatedPivot:Pivot, nextPivotToUpdate:Pivot, toIncluded: Int, additionalFAppliedBefore: LinearTransform, accumulatingTransformation: RedBlackTree[Pivot]):RedBlackTree[Pivot] = {
    if (nextPivotToUpdate.fromValue == toIncluded+1) return accumulatingTransformation //finished the correction

    val applyingOriginalPivot = pivotWithPositionApplyingTo(nextPivotToUpdate.fromValue).head.value

    val updatedPivot = Pivot(nextPivotToUpdate.fromValue, applyingOriginalPivot.f)




    null
  }

  def pivotWithPositionApplyingTo(value:Int):Option[RBPosition[Pivot]] = {
    transformation.getBiggestLowerOrEqual(value) match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }
  def pivotApplyingTo(value:Int):Option[Pivot] = {
    transformation.getBiggestLowerOrEqual(value)  match {
      case None => None
      case Some(_, p) => Some(p)
    }
  }

  def firstPivotAndPosition:Option[RBPosition[Pivot]] = {
    transformation.getSmallest match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }

  def lastPivotAndPosition:Option[RBPosition[Pivot]] = {
    transformation.getBiggest match{
      case None => None
      case Some((fromValueOfPivot,_)) => transformation.positionOf(fromValueOfPivot)
    }
  }
}

class Pivot(val fromValue:Int, val f: LinearTransform){
  override def toString = "Pivot(from:" + fromValue + " " + f + ")"
}
