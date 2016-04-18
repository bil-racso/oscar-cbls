package oscar.cbls.invariants.core.algo.seq.functional

import oscar.cbls.invariants.core.algo.fun.functional.{Pivot, PiecewiseLinearBijectionNaive}
import oscar.cbls.invariants.core.algo.rb.RBPosition

/*
class IntSequence(private[seq] val internalPositionToValue:Array[Int],
                  private[seq] val valueToInternalPosition:Array[Int],
                  private[seq] val internalToExternalPosition:PiecewiseLinearBijectionNaive)
  extends Iterable[Int] {

  def valueAtPosition(position:Int):Int = internalPositionToValue(internalToExternalPosition.backward(position))

  def positionOfValue(value:Int):Int = internalToExternalPosition.forward(valueToInternalPosition(value))

  def crawlerAtPosition(position:Int):IntSequenceCrawler = IntSequenceCrawler(this,position)

  def insertAfter(value:Int, pos:Int)
  def insertBefore(value:Int, pos:Int)
  def moveAfter(startPosition:Int, endPosition:Int, moveAfterPosition:Int, flip:Boolean)
}


object IntSequenceCrawler{
  def apply(s:IntSequence,position:Int):IntSequenceCrawler = {
    s.internalToExternalPosition.backward.pivotApplyingTo(position) match{
      case None =>

      case Some(pivot) =>
        val currentPivotPosition = s.internalToExternalPosition.backward.positionOfValue(position).head
        val pivotAbovePosition = currentPivotPosition.next
        val pivotBelowPosition = currentPivotPosition.prev

        new IntSequenceCrawler(s,
          position,
          pivotBelowPosition,
          currentPivotPosition,
          pivotAbovePosition)
    }
  }
}

class IdentityCrawler(sequence:IntSequence, position:Int)
  extends IntSequenceCrawler(sequence,position,null,null,null){
  override def next:Option[IdentityCrawler]
  override def prev:Option[IdentityCrawler]

}



class IntSequenceCrawler(sequence:IntSequence,
                         val position:Int,
                         pivotBelowPosition:Option[RBPosition[Pivot]],
                         currentPivotPosition: RBPosition[Pivot],
                         pivotAbovePosition:Option[RBPosition[Pivot]]){

  val currentPivot = currentPivotPosition.value
  val value:Int = sequence.internalPositionToValue(currentPivot.f(position))

  def next:Option[IntSequenceCrawler] = {
    if(currentPivot.f.minus){

    }else{
      if(position == currentPivot.fromValue){

      }

    }
  }
  def prev:Option[IntSequenceCrawler]


}
*/