package oscar.cbls.invariants.core.algo.seq.functional

import oscar.cbls.invariants.core.algo.fun.functional.{PiecewiseLinearBijectionNaive, Pivot}
import oscar.cbls.invariants.core.algo.fun.mutable.LinearPositionTransform
import oscar.cbls.invariants.core.algo.rb.{RBPosition, RedBlackTree}

object UniqueIntSequence{
  def apply(maxValue:Int, maxPivot:Int = 10):UniqueIntSequence = new UniqueIntSequence(
    RedBlackTree.empty[Int],
    RedBlackTree.empty[Int],
    PiecewiseLinearBijectionNaive.identity,
    0,
    maxPivot
  )
}

class UniqueIntSequence(private[seq] val internalPositionToValue:RedBlackTree[Int],
                        private[seq] val valueToInternalPosition:RedBlackTree[Int],
                        private[seq] val externalToInternalPosition:PiecewiseLinearBijectionNaive,
                        private[seq] val startFreeRangeForInternalPosition:Int,
                        maxPivot:Int = 10)
  extends Iterable[Int] {

  override def size : Int = valueToInternalPosition.size

  override def iterator : Iterator[Int] = new UniqueIntsequenceIterator(this)

  def valueAtPosition(position : Int) : Option[Int] = {
    val internalPosition:Int = externalToInternalPosition.forward(position)
    internalPositionToValue.get(internalPosition)
  }

  def positionOfValue(value : Int) : Option[Int] = {
    valueToInternalPosition.get(value) match{
      case None => None
      case Some(internalPosition) => Some(externalToInternalPosition.backward(internalPosition))
    }
  }

  def isValueIncluded(value:Int):Boolean = valueToInternalPosition.contains(value)

  def crawlerAtPosition(position : Int) : Option[IntSequenceCrawler] = {
    if (position >= this.size) None
    else {
      val currentPivotPosition = externalToInternalPosition.forward.pivotApplyingTo(position)
      val (pivotAbovePosition,internalPosition) = currentPivotPosition match {
        case None => (None,position)
        case Some(p) => (p.next,p.value.f(position))
      }

      Some(new IntSequenceCrawler(this,
        position,
        internalPositionToValue.positionOf(internalPosition).head,
        currentPivotPosition,
        pivotAbovePosition)()
      )
    }
  }

  def crawlerAtValue(value: Int) : Option[IntSequenceCrawler] = {
    positionOfValue(value) match{
      case None => None
      case Some(position) => crawlerAtPosition(position)
    }
  }

  def insertAfter(value:Int, pos:Int):UniqueIntSequence = {
    //insert into red blacks
    val newInternalPositionToValue = internalPositionToValue.insert(startFreeRangeForInternalPosition,value)
    val newValueToInternalPosition = valueToInternalPosition.insert(value,startFreeRangeForInternalPosition)

    //move sequence after position one upward
    //move inserted point at its position
    val newExternalToInternalPosition = externalToInternalPosition.update(
      (pos+1,Int.MaxValue,LinearPositionTransform(-1,false)),
      (pos+1,pos+1,LinearPositionTransform(pos-startFreeRangeForInternalPosition,false)))

    new UniqueIntSequence(newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition+1,
      maxPivot)
  }

  def insertBefore(value:Int, pos:Int):UniqueIntSequence = {
    //insert into red blacks
    val newInternalPositionToValue = internalPositionToValue.insert(startFreeRangeForInternalPosition,value)
    val newValueToInternalPosition = valueToInternalPosition.insert(value,startFreeRangeForInternalPosition)

    //move sequence after position one upward
    //move inserted point at its position
    val newExternalToInternalPosition = externalToInternalPosition.update(
      (pos+1,Int.MaxValue,LinearPositionTransform(-1,false)),
      (pos+1,pos+1,LinearPositionTransform(pos-startFreeRangeForInternalPosition,false)))

    new UniqueIntSequence(newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition+1,
      maxPivot)
  }

  def moveAfter(startPosition:Int, endPosition:Int, moveAfterPosition:Int, flip:Boolean):UniqueIntSequence = null
  def regularize:UniqueIntSequence = null
}

class UniqueIntsequenceIterator(s:UniqueIntSequence) extends Iterator[Int] {
  var crawler = s.crawlerAtPosition(0)

  override def hasNext : Boolean =
    crawler match{
      case None => false
      case Some(_) => true}

  override def next() : Int = {
    val position = crawler.head
    crawler = position.next
    position.value
  }
}


class IntSequenceCrawler(sequence:UniqueIntSequence,
                         val position:Int,
                         positionInRB:RBPosition[Int],
                         currentPivotPosition:Option[RBPosition[Pivot]],
                         pivotAbovePosition:Option[RBPosition[Pivot]])(
                          limitAboveForCurrentPivot:Int = pivotAbovePosition match{
                            case None => sequence.externalToInternalPosition.forward.firstPivot.head._1
                            case Some(p) => p.value.fromValue-1},
                          limitBelowForCurrentPivot:Int = currentPivotPosition match{
                            case None => Int.MinValue
                            case Some(p) => p.value.fromValue},
                          slopeIsPositive:Boolean = currentPivotPosition match{
                            case None => true
                            case Some(p) => !p.value.f.minus}
                          ) {

  val value : Int = positionInRB.value

  def next : Option[IntSequenceCrawler] = {
    if(position == limitAboveForCurrentPivot){
      //change pivot, we are also sure that there is a next, so use .head

      val newPivotAbovePosition = pivotAbovePosition.head.next
      val newPosition = position + 1
      val newPositionInRB = sequence.internalPositionToValue.positionOf(newPosition).head

      Some(new IntSequenceCrawler(sequence,
        newPosition,
        newPositionInRB,
        pivotAbovePosition,
        newPivotAbovePosition)(limitBelowForCurrentPivot = newPosition))

    }else{
      //do not change pivot

      (if(slopeIsPositive) positionInRB.next else positionInRB.prev) match{
        case None => None
        case Some(newPositionInRB) =>
          Some(new IntSequenceCrawler(sequence,
            position + 1,
            newPositionInRB,
            currentPivotPosition,
            pivotAbovePosition)(
            limitAboveForCurrentPivot,
            limitBelowForCurrentPivot,
            slopeIsPositive))
      }
    }
  }

  def prev : Option[IntSequenceCrawler] = {
    if (position == 0) None
    else if (position  == limitBelowForCurrentPivot) {
      //change pivot
      val newPosition = position -1
      val newCurrentPivotPosition = currentPivotPosition.head.prev
      val newInternalPosition = newCurrentPivotPosition.head.value.f(newPosition)
      val newCurrentPositionInRB = sequence.internalPositionToValue.positionOf(newInternalPosition).head
      Some(new IntSequenceCrawler(sequence,
        newPosition,
        newCurrentPositionInRB,
        newCurrentPivotPosition,
        currentPivotPosition)(limitAboveForCurrentPivot = limitBelowForCurrentPivot-1
      ))
    }else{
      //do not change pivot
      Some(new IntSequenceCrawler(sequence,
        position-1,
        if(slopeIsPositive) positionInRB.prev.head else positionInRB.next.head,
        currentPivotPosition,
        pivotAbovePosition)(
        limitAboveForCurrentPivot,
        limitBelowForCurrentPivot,
        slopeIsPositive))
    }
  }
}

