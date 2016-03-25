package oscar.cbls.invariants.core.algo.seq

import oscar.cbls.invariants.core.algo.rb.RedBlackTree

/*
abstract class UniqueIntSequenceWithPosition(maxSize:Int) extends UniqueIntSequence(maxSize){


  private val valueToUncorrectedPosition:Array[Int] = Array.fill[Int](maxSize)(-1)

  private val uncorrectedPositionToValue:Array[Int] = Array.fill[Int](maxSize)(-1)


  //internal position => external position
  private var uncorrectedToCorrectedPosition:RedBlackTree[Pivot] = RedBlackTree.empty


  def correctForInsert(value:Int,after:Int){

  }
  def valueAtPosition(position:Int):Int = {
    correctedToUncorrectedPosition.getBiggestLower(position) match{
      case None => uncorrectedPositionToValue(position)
      case Some(pivot) => uncorrectedPositionToValue(pivot.correction(position))
    }
  }

  def regularizePositions

  def positionOfValue(value:Int):Int = {
    val uncorrectedPoition:Int = valueToUncorrectedPosition(value)
    uncorrectedToCorrectedPosition.getBiggestLower(uncorrectedPoition) match{
      case None =>  uncorrectedPoition
      case Some(pivot) => pivot.correction(uncorrectedPoition)
    }
  }


}
*/

object TestUpdateableFunction extends App{
  val fn = new UpdateableFunction()

  println("init:" + fn)

  fn.updateFunction(6, 20, new LinearPositionTransform(-3,true))
  println(fn)
  println
  println


  fn.updateFunction(7, 14, new LinearPositionTransform(3,false))
  println(fn)
  println
  println

  fn.updateFunction(16, 19, new LinearPositionTransform(5,true))
  println(fn)
  println
  println



  fn.updateFunction(7, 14, new LinearPositionTransform(-3,false))
  println(fn)
  println
  println

  fn.updateFunction(6, 20, new LinearPositionTransform(-3,true))
  println(fn)
  println
  println

}

class UpdateableFunction() {
  //external position => internal position
  private var transformation: RedBlackTree[Pivot] = RedBlackTree.empty


  override def toString: String = {
    "nbPivots:" + transformation.size + " \n" +
      (transformation.getSmallestBigger(Int.MinValue) match{
      case None => "identity"
      case Some((_,minPivot)) => minPivot.toStringAll
    })
  }

  def updateFunction(correctedFrom: Int, correctedTo: Int, correctedToUncorrectedCorrection: LinearPositionTransform): Unit = {
    println("updateFunction(from:" + correctedFrom + ", to:" + correctedTo + ", fct:" + correctedToUncorrectedCorrection + ")")
    transformation.getBiggestLower(correctedFrom) match {
      case Some((_,pivot)) if (pivot.value == correctedFrom) =>
        updateFromPivot(pivot, correctedTo, correctedToUncorrectedCorrection)
      case Some((_,pivot)) =>
        //there is a pivot below the point
      //need to add an intermediary pivot, ans relink to this one
        val next = pivot.next
        val newPivot = new Pivot(correctedFrom, null, null, pivot.correction)
        transformation = transformation.insert(correctedFrom, newPivot)
        pivot.setNextAndRelink(newPivot)
        newPivot.setNextAndRelink(next)
        updateFromPivot(newPivot, correctedTo, correctedToUncorrectedCorrection)
      case None =>
        transformation.getSmallestBigger(correctedFrom) match{
          case None =>
            //need to add a first pivot from this point
            val newPivot = new Pivot(correctedFrom, null, null, LinearPositionTransform.identity)
            transformation = transformation.insert(correctedFrom, newPivot)
            updateFromPivot(newPivot, correctedTo, correctedToUncorrectedCorrection)
          case Some((_,next)) =>
            val newPivot = new Pivot(correctedFrom, null, null, LinearPositionTransform.identity)
            transformation = transformation.insert(correctedFrom, newPivot)
            newPivot.setNextAndRelink(next)
            updateFromPivot(newPivot, correctedTo, correctedToUncorrectedCorrection)
        }
    }
  }

  def queryFunction(value:Int):Int = {
    transformation.getBiggestLower(value) match {
      case None => value
      case Some((_,pivot)) => pivot.correction(value)
    }
  }

  def updateFromPivot(pivot: Pivot, correctedTo: Int, correctedToUncorrectedCorrection: LinearPositionTransform) {
    val next = pivot.next
    val prev = pivot.prev
    val previousCorrection = pivot.correction
    val newPrev = if (pivot.update(correctedToUncorrectedCorrection)) {
      //should be removed
      pivot.removeFromDLL()
      transformation = transformation.remove(pivot.value)
      prev
    } else {
      pivot
    }
    if (pivot.value == correctedTo+1) return //finished the correction //TODO: exit condition seems wrong
    if (next == null) {
      //need to add a finishing pivot, to finish from correction from before
      if (newPrev == null) return
      //We have an open correction, and need to close it with the previous value previousCorrection
     //TODO: handle one more case of identity function properly
      val newPivot = new Pivot(correctedTo+1, null, null, previousCorrection)
      transformation = transformation.insert(correctedTo+1, newPivot)
      newPrev.setNextAndRelink(newPivot)
      return
    } else if (next.value > correctedTo +1) {
      //need to add a new intermediary pivot
      if (newPrev == null) return
      if (newPrev.correction.equals(previousCorrection)) return
      val newPivot = new Pivot(correctedTo+1, null, null, previousCorrection)
      transformation = transformation.insert(correctedTo+1, newPivot)
      newPrev.setNextAndRelink(newPivot)
      newPivot.setNextAndRelink(next)
      return
    } else if (next.value < correctedTo+1){
      //there is a next such that next.value is <= correctedTo
      //so recurse to it
      updateFromPivot(next, correctedTo, correctedToUncorrectedCorrection)
    }else{
      //check that nexnextt pivot should not be removed, actually

      if((newPrev == null && next.correction.isIdentity)
        || next.correction.equals(newPrev.correction)){
        //next can be removed
        next.removeFromDLL()
        transformation = transformation.remove(next.value)
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
}

class Pivot(val value:Int, var next:Pivot = null, var prev:Pivot, var correction: LinearPositionTransform){

  override def toString = "Pivot(from:" + value + " correction:" + correction + ")"

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
    correction = transformPerformedAfter(correction)
    if(prev == null) return (correction.isIdentity)
    return prev.correction.equals(correction)
  }
}

/*
problème: on se base ici sur un index non brisé, or il a éé brisé par les mouvements opérés...)
le bon index linéaire est celui après transformation
 */