package oscar.cbls.invariants.core.algo.fun

import oscar.cbls.invariants.core.algo.rb.RedBlackTree

object PiecewiseLinearFun{
  def identity = new PiecewiseLinearFun()
}

class PiecewiseLinearFun(transformation: RedBlackTree[Segment] = RedBlackTree.empty) {

  def firstPivot:Option[(Int,Segment)] = transformation.getSmallestBigger(Int.MinValue)

  override def toString: String = {
    "PiecewiseLinearFun(nbSegments:" + transformation.size + ", " + (if(transformation.isEmpty) "identity" else ("segments:" + transformation.values.mkString(",")))+")"
  }

  def apply(value:Int):Int = {
    transformation.getBiggestLower(value) match {
      case None => value
      case Some((_,pivot)) => pivot.f(value)
    }
  }

  def update(fromIncluded: Int, toIncluded: Int, additionalF: LinearPositionTransform): PiecewiseLinearFun = {
    println("updateFunction(from:" + fromIncluded + ", to:" + toIncluded + ", fct:" + additionalF + ")")
    transformation.getBiggestLower(fromIncluded) match {
      case Some((_,pivot)) if (pivot.value == fromIncluded) =>
        updateFromPivot(pivot, toIncluded, additionalF)
      case Some((_,pivot)) =>
        //there is a pivot below the point
        //need to add an intermediary pivot, ans relink to this one
        val next = pivot.next
        val newPivot = createNewPivot(fromIncluded, null, null, pivot.f)
        transformation = transformation.insert(fromIncluded, newPivot)
        pivot.setNextAndRelink(newPivot)
        newPivot.setNextAndRelink(next)
        insertedPivot(newPivot)
        updateFromPivot(newPivot, toIncluded, additionalF)
      case None =>
        transformation.getSmallestBigger(fromIncluded) match{
          case None =>
            //need to add a first pivot from this point
            val newPivot = createNewPivot(fromIncluded, null, null, LinearPositionTransform.identity)
            transformation = transformation.insert(fromIncluded, newPivot)
            insertedPivot(newPivot)
            updateFromPivot(newPivot, toIncluded, additionalF)
          case Some((_,next)) =>
            val newPivot = createNewPivot(fromIncluded, null, null, LinearPositionTransform.identity)
            transformation = transformation.insert(fromIncluded, newPivot)
            newPivot.setNextAndRelink(next)
            insertedPivot(newPivot)
            updateFromPivot(newPivot, toIncluded, additionalF)
        }
    }
  }

  private def updateFromPivot(pivot: Pivot, toIncluded: Int, additionalF: LinearPositionTransform) {
    val next = pivot.next
    val prev = pivot.prev
    val previousCorrection = pivot.f
    val newPrev = if (pivot.update(additionalF)) {
      //should be removed
      pivot.removeFromDLL()
      transformation = transformation.remove(pivot.value)
      deletedPivot(pivot)
      prev
    } else {
      updatedPivot(pivot)
      pivot
    }
    if (pivot.value == toIncluded+1)return //finished the correction
    if (next == null) {
      //need to add a finishing pivot, to finish from correction from before
      if (newPrev == null) return
      //We have an open correction, and need to close it with the previous value previousCorrection
      val newPivot = createNewPivot(toIncluded+1, null, null, previousCorrection)
      transformation = transformation.insert(toIncluded+1, newPivot)
      newPrev.setNextAndRelink(newPivot)
      insertedPivot(newPivot)
      return
    } else if (next.value > toIncluded +1) {
      //need to add a new intermediary pivot
      if (newPrev == null) return
      if (newPrev.f.equals(previousCorrection)) return
      val newPivot = createNewPivot(toIncluded+1, null, null, previousCorrection)
      transformation = transformation.insert(toIncluded+1, newPivot)
      newPrev.setNextAndRelink(newPivot)
      newPivot.setNextAndRelink(next)
      insertedPivot(newPivot)
      return
    } else if (next.value < toIncluded+1){
      //there is a next such that next.value is <= correctedTo
      //so recurse to it
      updateFromPivot(next, toIncluded, additionalF)
    }else{
      //check that next pivot should not be removed, actually
      if((newPrev == null && next.f.isIdentity)
        || next.f.equals(newPrev.f)){
        //next can be removed
        next.removeFromDLL()
        transformation = transformation.remove(next.value)
        deletedPivot(next)
      }
    }
  }
}

class Segment(val fromValue:Int, var f: LinearPositionTransform){

  override def toString = "Pivot(from:" + value + " f:" + f + ")"

  def firstPivot:Pivot = if(prev == null) this else prev.firstPivot

  def removeFromDLL(){
    if(next!= null) next.prev = prev
    if(prev != null) prev.next = next
  }

  def setNextAndRelink(that:Pivot): Unit ={
    this.next = that
    if(that != null) that.prev = this
  }

  /**
   * @param transformPerformedAfter
   * @return true if should be removed, with respect to prev if exists, false otherwise
   *         so you must first update the prev to take this into account!
   */
  def update(transformPerformedAfter: LinearPositionTransform):Boolean = {
    f = transformPerformedAfter(f)
    if(prev == null) return (f.isIdentity)
    return prev.f.equals(f)
  }
}
