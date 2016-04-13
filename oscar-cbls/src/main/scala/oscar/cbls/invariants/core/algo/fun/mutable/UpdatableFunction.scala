package oscar.cbls.invariants.core.algo.fun.mutable

import oscar.cbls.invariants.core.algo.rb.RedBlackTree

class PiecewiseLinearFunction() {
  //external position => internal position
  protected var transformation: RedBlackTree[Pivot] = RedBlackTree.empty

  def firstPivot:Option[(Int,Pivot)] = transformation.getSmallestBiggerOrEqual(Int.MinValue)

  def lastPivot:Option[(Int,Pivot)] = transformation.getBiggestLowerOrEqual((Int.MaxValue))

  def clearAndSetPivots(l:List[Pivot]){
    setAsIdentity()
    val sortedPivots = l.sortBy(_.value)
    var sortedPivotForRelink = sortedPivots
    while(sortedPivotForRelink.nonEmpty){
      sortedPivotForRelink match {
        case h1 :: h2 :: t =>
          h1.setNextAndRelink(h2)
          sortedPivotForRelink = h2 :: t
        case _ => sortedPivotForRelink = List.empty
      }
    }
    for(p <- sortedPivots){
      transformation.insert(p.value,p)
    }
  }

  override def toString: String = {
    "nbPivots:" + transformation.size + " \n" +
      (firstPivot match{
        case None => "identity"
        case Some((_,minPivot)) => minPivot.toStringAll
      })
  }

  def apply(value:Int):Int = {
    transformation.getBiggestLowerOrEqual(value) match {
      case None => value
      case Some((_,pivot)) => pivot.f(value)
    }
  }



  protected def updatedPivot(p:Pivot){}
  protected def deletedPivot(p:Pivot){}
  protected def insertedPivot(p:Pivot){}
  protected def createNewPivot(value:Int, next:Pivot = null, prev:Pivot, f: LinearPositionTransform):Pivot = new Pivot(value, next, prev, f)

  def setAsIdentity(){
    transformation = RedBlackTree.empty
  }

  def update(fromIncluded: Int, toIncluded: Int, additionalF: LinearPositionTransform): Unit = {
    println("updateFunction(from:" + fromIncluded + ", to:" + toIncluded + ", fct:" + additionalF + ")")
    transformation.getBiggestLowerOrEqual(fromIncluded) match {
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
        transformation.getSmallestBiggerOrEqual(fromIncluded) match{
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

object LinearPositionTransform{
  val identity = new LinearPositionTransform(0,false)
}

/**
 * linear transformer of position.
 * value => offset op value where op is + or minus, according to "boolean flip": true => - fase => +
 */
class LinearPositionTransform(val offset:Int,val minus:Boolean){
  def apply(value:Int) = if(minus) offset - value else offset + value
  def unApply(value:Int) = if(minus) offset - value else value - offset

  /**
   * delivers a new linear transform that is equal to this(that(value))
   * @param that
   * @return
   */
  def apply(that: LinearPositionTransform):LinearPositionTransform = {
    new LinearPositionTransform(this(that.offset),this.minus != that.minus)
  }

  def equals(that:LinearPositionTransform):Boolean = {
    this.offset == that.offset && this.minus == that.minus
  }

  def isIdentity:Boolean = offset == 0 && !minus

  override def toString: String = (
    if(offset == 0) {
      if (minus) "(x=>-x)"
      else "(x=>x)"
    }else "(x=>" + offset + (if (minus) "-" else "+") + "x)")

  //this: if minus y = offset - x else y = offset + x
  //that : if minus x = offset - y else x = - offset + y
  def invert:LinearPositionTransform = new LinearPositionTransform(if(minus) offset else (-offset),minus)
}

class Pivot(val value:Int, var next:Pivot = null, var prev:Pivot, var f: LinearPositionTransform){

  override def toString = "Pivot(from:" + value + " f:" + f + ")"

  def toStringAll:String = this.toString + (if(next==null) "" else "\n" + next.toStringAll)

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
