package oscar.cbls.invariants.core.algo.seq.functional

import oscar.cbls.invariants.core.algo.fun.{PiecewiseLinearBijectionNaive, Pivot, LinearTransform}
import oscar.cbls.invariants.core.algo.rb.{RBPosition, RedBlackTree}

object UniqueIntSequence{
  def apply(maxPivot:Int = 10, maxSize:Int = 1000):UniqueIntSequence = new UniqueIntSequence(
    RedBlackTree.empty[Int],
    RedBlackTree.empty[Int],
    PiecewiseLinearBijectionNaive.identity,
    0,
    maxPivot,
  maxSize
  )
}

class UniqueIntSequence(private[seq] val internalPositionToValue:RedBlackTree[Int],
                        private[seq] val valueToInternalPosition:RedBlackTree[Int],
                        private[seq] val externalToInternalPosition:PiecewiseLinearBijectionNaive,
                        private[seq] val startFreeRangeForInternalPosition:Int,
                        maxPivot:Int = 10, maxSize:Int = 1000) {

  override def toString : String = {
    "UniqueIntSequence(\n\tinternalPositionToValue:" + internalPositionToValue.content +
    "\n\tvalueToInternalPosition:" + valueToInternalPosition.content +
      "\n\texternalToInternalPosition:" + externalToInternalPosition + "\n)"
  }

  def size : Int = valueToInternalPosition.size

  def iterator : Iterator[Int] = new UniqueIntSequenceIterator(this)

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
      val currentPivotPosition = externalToInternalPosition.forward.pivotWithPositionApplyingTo(position)
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

  def insertAtPosition(value:Int, pos:Int):UniqueIntSequence = {
    require(pos<=size,"inserting past the end of the sequence (size:" + size + " pos:" + pos + ")")
    //insert into red blacks
    val newInternalPositionToValue = internalPositionToValue.insert(startFreeRangeForInternalPosition,value)
    val newValueToInternalPosition = valueToInternalPosition.insert(value,startFreeRangeForInternalPosition)

    //move sequence after position, one upward
    //move inserted point at its position
    val oldExternalPosRelatedToFreeInternalPos = externalToInternalPosition.backward(startFreeRangeForInternalPosition)

    println("old external pointing to internalFreeValue:" + oldExternalPosRelatedToFreeInternalPos)
    println("startFreeRangeForInternalPosition:" + startFreeRangeForInternalPosition)
    val newExternalToInternalPosition = if(pos == size) {
      //inserting at end of the sequence
      println("inserting at end of the sequence")
      externalToInternalPosition.updateBefore(
        (startFreeRangeForInternalPosition,startFreeRangeForInternalPosition,LinearTransform(pos - oldExternalPosRelatedToFreeInternalPos,false)))
    }else{
      //inserting somewhere within the sequence, need to shift upper part
      externalToInternalPosition.updateBefore(
        (pos+1,size,LinearTransform(+1,false)),
        (startFreeRangeForInternalPosition,startFreeRangeForInternalPosition,LinearTransform(pos - oldExternalPosRelatedToFreeInternalPos,false)))
    }

    new UniqueIntSequence(
      newInternalPositionToValue,
      newValueToInternalPosition,
      newExternalToInternalPosition,
      startFreeRangeForInternalPosition+1,
      maxPivot,maxSize)
  }

  def moveAfter(startPosition:Int, endPosition:Int, moveAfterPosition:Int, flip:Boolean):UniqueIntSequence = null
  def regularize:UniqueIntSequence = null
}

class UniqueIntSequenceIterator(s:UniqueIntSequence) extends Iterator[Int] {
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

