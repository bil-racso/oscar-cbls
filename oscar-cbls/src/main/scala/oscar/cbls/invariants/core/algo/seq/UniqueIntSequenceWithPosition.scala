package oscar.cbls.invariants.core.algo.seq

import oscar.cbls.invariants.core.algo.fun.{LinearPositionTransform, UpdateableBijection}

class UniqueIntSequenceWithPosition(maxVal:Int) extends UniqueIntSequence(maxVal){

  private val valueToInternalPosition:Array[Int] = Array.fill[Int](maxVal+1)(-1)
  private val internalPositionToValue:Array[Int] = Array.fill[Int](maxVal+1)(-1)

  //internal position => external position
  private var externalToInternalPosition:UpdateableBijection = new UpdateableBijection

  def valueAtPosition(position:Int):Int = {
    internalPositionToValue(externalToInternalPosition(position))
  }

  def positionOfValue(value:Int):Int = {
    externalToInternalPosition.unApply(valueToInternalPosition(value))
  }

  def flushBinjectionToArrays(){

  }

  def updateBijectionAndArrayForInsert(){
    //insert at the end of the sequence, and perform a move

  }

  def updateBijjectionAndArrayForDelete(position:Int){
    //we do not update the arrays, actually.

  }

  /**
   *
   * @param fromIncludedPosition
   * @param toIncludedPosition
   * @param afterPosition
   * @param flip
   */
  def updateBijectionAndArrayForMove(fromIncludedPosition:Int,toIncludedPosition:Int,afterPosition:Int,flip:Boolean){
    //we do not update the arrays, actually.
    require(afterPosition < fromIncludedPosition || toIncludedPosition < afterPosition)
    require(fromIncludedPosition <= toIncludedPosition)
    if(afterPosition+1 == fromIncludedPosition){
      //only a flip
      externalToInternalPosition.update(
        fromIncludedPosition,
        toIncludedPosition,
        new LinearPositionTransform(fromIncludedPosition + toIncludedPosition,true))
    }else if(afterPosition<fromIncludedPosition){
      //moving the segment towards beginning of sequence
      val startOtherSegmentExternalPosition = afterPosition+1
      val endOtherSegmentExternalPosition = fromIncludedPosition -1

      externalToInternalPosition.update(
        startOtherSegmentExternalPosition,
        endOtherSegmentExternalPosition,
        new LinearPositionTransform(toIncludedPosition - fromIncludedPosition,false))

      externalToInternalPosition.update(
        fromIncludedPosition,
        toIncludedPosition,
        new LinearPositionTransform(fromIncludedPosition - afterPosition,flip))

    }else{
      //moving the segment towards the endof the sequence
    }
  }
}

abstract class ImmutableIntSequence extends Iterable[Int] {
  def position(value:Int)
  def value(position:Int)
}

object TestUniqueSequence extends App{
  val a = new UniqueIntSequenceWithPosition(20)

  println("before: " + a)

  a.insertAfter(0)
  a.updateBijectionAndArrayForMove(4,10,15,true)
  println("after: " + a)

}

/*
problème: on se base ici sur un index non brisé, or il a éé brisé par les mouvements opérés...)
le bon index linéaire est celui après transformation
 */

